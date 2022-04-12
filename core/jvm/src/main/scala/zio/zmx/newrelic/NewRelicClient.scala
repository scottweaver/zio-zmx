package zio.zmx.newrelic

import zio._
import zio.json.ast._
trait NewRelicClient {

  // val env = ChannelFactory.auto ++ EventLoopGroup.auto()
  // val url = "http://sports.api.decathlon.com/groups/water-aerobics"

  // val program = for {
  //   res  <- Client.request(url)
  //   data <- res.bodyAsString
  //   _    <- Console.printLine(data)
  // } yield ()

  //   ???

  // }
  def sendMetrics(json: Chunk[Json]): ZIO[Any, Throwable, Unit] 
}

final case class LiveNewRelicClient() extends NewRelicClient {
  def sendMetrics(json: Chunk[Json]): ZIO[Any, Throwable, Unit] = {

    val body = Json.Arr(
      Json.Obj("metrics" ->Json.Arr(json))
    ).toString 

    // val url = "https://insights-collector.newrelic.com/v1/accounts/82601/events"
    // val headers = Map(
    //   "X-Insert-Key" -> "f8f8f8f8-f8f8-f8f8-f8f8-f8f8f8f8f8f8",
    //   "Content-Type" -> "application/json"
    // )
    // val body = Json.obj(
    //   "events" -> json
    // )
    // val request = Request(
    //   method = Method.POST,
    //   url = url,
    //   headers = headers,
    //   body = body
    // )
    // val client = Client.default
    // client.request(request).map(_ => ())
    ???
  }
}
