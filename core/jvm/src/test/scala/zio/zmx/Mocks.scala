package zio.zmx

import java.util.concurrent.atomic.AtomicReference

import zio._
import zio.URLayer
import zio.json.ast.Json
import zio.metrics.MetricPair
import zio.mock._
import zio.zmx.newrelic._

object Mocks {

  // object MockNewRelicClient extends Mock[NewRelicClient] {

  //   object SendMetrics extends Effect[Chunk[Json], Throwable, Unit]
  //   override val compose: URLayer[Proxy, NewRelicClient] =
  //     ZIO
  //       .serviceWith[Proxy] { proxy =>
  //         new NewRelicClient {
  //           def sendMetrics(json: Chunk[Json]): ZIO[Any, Throwable, Unit] =
  //             proxy(SendMetrics, json)

  //         }
  //       }
  //       .toLayer
  // }

  // object MockNewRelicEncoder extends Mock[NewRelicEncoder] {
  //   object EncodeMetrics extends Method[(Chunk[MetricPair.Untyped], Long), Throwable, Chunk[Json]]

  //   override val compose: URLayer[Proxy, NewRelicEncoder] =
  //     ZIO
  //       .serviceWithZIO[Proxy] { proxy =>
  //         withRuntime[Any].map { rts =>
  //           new NewRelicEncoder {
  //             def encodeMetrics(metrics: Chunk[MetricPair.Untyped], timestamp: Long): Chunk[Json] =
  //               rts.unsafeRun(proxy(EncodeMetrics, metrics -> timestamp))
  //           }
  //         }
  //       }
  //       .toLayer

  // }

  final case class MockNewRelicClient(private val recording: Ref[Chunk[Json]]) extends NewRelicClient {
    override def sendMetrics(json: Chunk[Json]): ZIO[Any, Throwable, Unit] =
      recording.update(_ ++ json)

    def state = recording.get
  }

  object MockNewRelicClient {
    val mock = Ref.make(Chunk.empty[Json]).map(new MockNewRelicClient(_)).toLayer
  }

  final case class MockNewRelicEncoder() extends NewRelicEncoder {

    val recording: AtomicReference[Chunk[(Chunk[MetricPair.Untyped], Long)]] = new AtomicReference(
      Chunk.empty[(Chunk[MetricPair.Untyped], Long)],
    )

    override def encodeMetrics(metrics: Chunk[MetricPair.Untyped], timestamp: Long): Chunk[Json] = {
      recording.getAndUpdate(_ :+ (metrics -> timestamp))
      Chunk.empty
    }

    def state = recording.get
  }

  object MockNewRelicEncoder {
    def mock = ZLayer.succeed(MockNewRelicEncoder())
  }

  final case class MockNewRelicPublisher() extends NewRelicPublisher {

    private val recording: AtomicReference[Chunk[MetricPair.Untyped]]   = new AtomicReference(
      Chunk.empty[MetricPair.Untyped],
    )
    def runPublisher: ZIO[Any, Nothing, Fiber[Throwable, Unit]] = ZIO.succeed(Fiber.succeed(()))

    def unsafePublish(pair: MetricPair.Untyped): Unit = {
      recording.getAndUpdate(_ :+ pair)
      ()
    }

    def state = recording.get

  }

  object MockNewRelicPublisher {
    def mock = ZLayer.succeed(MockNewRelicPublisher())
  }

}
