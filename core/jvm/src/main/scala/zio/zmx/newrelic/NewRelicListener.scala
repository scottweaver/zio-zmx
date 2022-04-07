package zio.zmx.newrelic

import zio._
import zio.internal.metrics._
import zio.metrics._

trait NewRelicListener extends MetricListener {
  def unsafeUpdate[Type <: MetricKeyType](key: MetricKey[Type]): key.keyType.In => Unit
}

object NewRelicListener {

  val live =
    ZLayer.fromZIO(
      for {
        publisher <- ZIO.service[NewRelicPublisher]
      } yield LiveNewRelicListener(publisher),
    )

}

final case class LiveNewRelicListener private[newrelic] (publisher: NewRelicPublisher) extends NewRelicListener {

  override def unsafeUpdate[Type <: MetricKeyType](key: MetricKey[Type]): key.keyType.In => Unit = { in =>
    println(s"::: > NewRelicListener.unsafeUpdate `key.keyType.In` = '$in'")
    key.keyType match {
      case _: MetricKeyType.Counter   =>
        val counterKey = key.asInstanceOf[MetricKey[MetricKeyType.Counter]]
        val count      = in.asInstanceOf[MetricKeyType.Counter.In]
        val pair       = MetricPair.unsafeMake(counterKey, MetricState.Counter(count))
        publisher.unsafePublish(pair)
      case _: MetricKeyType.Gauge     =>
        val counterKey = key.asInstanceOf[MetricKey[MetricKeyType.Gauge]]
        val gaugeState       = metricRegistry.get[MetricKeyType.Gauge](counterKey).get()
        val pair       = MetricPair.unsafeMake(counterKey, gaugeState)
        publisher.unsafePublish(pair)
      case _: MetricKeyType.Frequency =>
        val counterKey = key.asInstanceOf[MetricKey[MetricKeyType.Frequency]]
        val hook       = metricRegistry.get[MetricKeyType.Frequency](counterKey)
        val pair       = MetricPair.unsafeMake(counterKey, hook.get())
        publisher.unsafePublish(pair)
      case _: MetricKeyType.Histogram =>
        val counterKey = key.asInstanceOf[MetricKey[MetricKeyType.Histogram]]
        val hook       = metricRegistry.get[MetricKeyType.Histogram](counterKey)
        val pair       = MetricPair.unsafeMake(counterKey, hook.get())
        publisher.unsafePublish(pair)
      case _: MetricKeyType.Summary   =>
        val counterKey = key.asInstanceOf[MetricKey[MetricKeyType.Summary]]
        val hook       = metricRegistry.get[MetricKeyType.Summary](counterKey)
        val pair       = MetricPair.unsafeMake(counterKey, hook.get())
        publisher.unsafePublish(pair)

    }
  }

  // override def unsafeUpdate[Type <: MetricKeyType](key: MetricKey[Type]): key.keyType.In => Unit = (key.keyType match {
  //   case _: MetricKeyType.Counter =>
  //     val counterKey = key.asInstanceOf[MetricKey[MetricKeyType.Counter]]
  //     val hook0      = metricRegistry.get[MetricKeyType.Counter](counterKey)
  //     val hook       = metricRegistry.get[MetricKeyType.Counter](counterKey)
  //     val pair       = MetricPair.unsafeMake(counterKey, hook.get())
  //     publisher.unsafePublish(pair)
  //     println(s"::: > LiveNewRelicListener.unsafeUpdate successfully published $pair")
  //   case _: MetricKeyType.Gauge =>
  //     val counterKey = key.asInstanceOf[MetricKey[MetricKeyType.Gauge]]
  //     val hook       = metricRegistry.get[MetricKeyType.Gauge](counterKey)
  //     val pair       = MetricPair.unsafeMake(counterKey, hook.get())
  //     publisher.unsafePublish(pair)
  //   case _: MetricKeyType.Frequency =>
  //     val counterKey = key.asInstanceOf[MetricKey[MetricKeyType.Frequency]]
  //     val hook       = metricRegistry.get[MetricKeyType.Frequency](counterKey)
  //     val pair       = MetricPair.unsafeMake(counterKey, hook.get())
  //     publisher.unsafePublish(pair)
  //   case _: MetricKeyType.Histogram =>
  //     val counterKey = key.asInstanceOf[MetricKey[MetricKeyType.Histogram]]
  //     val hook       = metricRegistry.get[MetricKeyType.Histogram](counterKey)
  //     val pair       = MetricPair.unsafeMake(counterKey, hook.get())
  //     publisher.unsafePublish(pair)
  //   case _: MetricKeyType.Summary =>
  //     val counterKey = key.asInstanceOf[MetricKey[MetricKeyType.Summary]]
  //     val hook       = metricRegistry.get[MetricKeyType.Summary](counterKey)
  //     val pair       = MetricPair.unsafeMake(counterKey, hook.get())
  //     publisher.unsafePublish(pair)

  // }).asInstanceOf[key.keyType.In => Unit]

}
