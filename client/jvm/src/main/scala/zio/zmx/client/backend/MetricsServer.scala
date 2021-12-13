package zio.zmx.client.backend

import uzhttp._
import uzhttp.server._

import zio._

import java.net.InetSocketAddress
import zio.metrics.jvm.DefaultJvmMetrics

import zio.zmx.notify.MetricNotifier

object MetricsServer extends ZIOAppDefault {

  private val bindHost        = "0.0.0.0"
  private val bindPort        = 8080
  private val stopServerAfter = 8.hours

  private val server = Server
    .builder(new InetSocketAddress(bindHost, bindPort))
    .handleSome {
      case req if req.uri.getPath.equals("/") => ZIO.succeed(Response.html("Hello Andreas!"))

      case req @ Request.WebsocketRequest(_, uri, _, _, inputFrames) if uri.getPath.startsWith("/ws") =>
        for {
          handler  <- ZIO.service[WSHandler]
          _        <- ZIO.logInfo(s"Handling WS request at <$uri>")
          appSocket = inputFrames.mapZIO(handler.handleZMXFrame).flatMap(_.flattenTake)
          response <- Response.websocket(req, appSocket)
        } yield response
    }
    .serve

  override def run = for {
    _ <- InstrumentedSample.program.fork
    s <- server.useForever.orDie.fork.provide(Clock.live, Random.live, WSHandler.live, MetricNotifier.live)
    f <- ZIO.unit.schedule(Schedule.duration(stopServerAfter)).fork
    _ <- f.join.flatMap(_ => s.interrupt)
  } yield ()
}

object MetricsServerWithJVMMetrics extends ZIOApp.Proxy(MetricsServer <> DefaultJvmMetrics.app)
