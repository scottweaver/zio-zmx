package zio.zmx.client.backend

import scala.util.Try

import zio._
import zio.stream._

import uzhttp.websocket._

import upickle.default._

import zio.zmx.client.ClientMessage
import zio.zmx.notify.MetricNotifier
import zio.zmx.client.MetricsUpdate

trait WSHandler {
  def handleZMXFrame(input: Frame): UIO[Stream[Nothing, Take[Nothing, Frame]]]
}

object WSHandler {
  val live: ZLayer[MetricNotifier, Nothing, WSHandler] = (for {
    notifier <- ZIO.service[MetricNotifier]
  } yield WSHandlerImpl(notifier)).toLayer

  case class WSHandlerImpl(
    notifier: MetricNotifier
  ) extends WSHandler {

    def handleZMXFrame(frame: Frame): UIO[Stream[Nothing, Take[Nothing, Frame]]] =
      for {
        cltMsg <- toClientMessage(frame).map(Some(_)).catchAll(_ => ZIO.none)
        _      <- ZIO.logInfo(s"Got message from client : <$cltMsg>")
        str    <- cltMsg match {
                    case None                               => ZIO.succeed(Stream.empty)
                    case Some(ClientMessage.Disconnect(id)) =>
                      notifier.disconnect(id).as(Stream(Take.single(Close), Take.end))
                    case Some(m)                            =>
                      ZIO.succeed(handleMessage(m).map(m => Take.single(Binary(write(m).getBytes()))))
                  }
      } yield str

    private def toClientMessage(frame: Frame): ZIO[Any, Any, ClientMessage] = frame match {
      case Binary(data, _)       => ZIO.fromTry(Try(read[ClientMessage](new String(data))))
      case Text(data, _)         => ZIO.fromTry(Try(read[ClientMessage](data)))
      case Continuation(data, _) => ZIO.fromTry(Try(read[ClientMessage](new String(data))))
      case _                     => ZIO.fail(())
    }

    private val handleMessage: ClientMessage => UStream[ClientMessage] = {
      case ClientMessage.Connect =>
        (ZStream
          .fromZIO(for {
            connected <- notifier.connect()
            states     = ZStream.from(ClientMessage.Connected(connected._1)) //++ connected._2.map(st =>
            //  ClientMessage.MetricsNotification(Chunk.fromIterable(st.map { case (k, s) =>
            //    MetricsUpdate.fromMetricState(k, s)
            //  }))
            //)
          } yield states))
          .flatten
      case _                     =>
        Stream.empty
    }
  }
}
