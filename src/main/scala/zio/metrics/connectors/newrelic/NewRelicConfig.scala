package zio.metrics.connectors.newrelic

import java.time.Duration

import zio._

final case class NewRelicConfig(
  apiKey: String,
  newRelicURI: NewRelicUri,
  maxMetricsPerRequest: Int,
  maxPublishingDelay: Duration)

object NewRelicConfig {

  // TODO: Use timecode to try and figure out the NR endpoint
  /**
   * Attempts to load the Settings from the environment.
   *
   * ===Environment Variables===
   *
   * | Name                                | Type          | Required                                                                                | Description                                            |
   * | ----------------------------------- | ------------- | --------------------------------------------------------------------------------------- | ------------------------------------------------------ |
   * | `NEW_RELIC_API_KEY`                 | string        | Yes                                                                                     | Your New Relic license key.                            |
   * | `NEW_RELIC_URI`                     | string        | No. Defaults to `https://metric-api.newrelic.com/metric/v1`.                            | The New Relic API endpoint appropriate for you region. |
   * | `NEW_RELIC_MAX_METRICS_PER_REQUEST` | integer       | No.  Defaults to 1000 (this the max number of metrics per request allowed by New Relic) | Maximum number of metrics to send in a single request. |
   * | `NEW_RELIC_MAX_PUBLISHING_DELAY`    | Java Duration | No.  Defaults to `PT5S` (5 seconds)                                                     | How long to wait before sending a metric request.      |
   *
   * REF: [[https://docs.newrelic.com/docs/data-apis/ingest-apis/metric-api/report-metrics-metric-api/#api-endpoint New Relic's Metric API Doc]]
   */
  val fromEnvLayer: ZLayer[Any, Nothing, NewRelicConfig] =
    ZLayer
      .fromZIO(for {
        apiKey               <- System.env(envApiKey).someOrFail(new IllegalArgumentException("APIKey is missing for New Relic"))
        newRelicUri          <- System.env(envMetricsUri).map(_.map(NewRelicUri.Custom.apply)).map(_.getOrElse(NewRelicUri.NA))
        maxMetricsPerRequest <- System.envOrElse(envMaxMetricsPerRequest, "1000").map(_.toInt)
        maxPublishingDelay   <- System.envOrElse(envMaxPublishingDelay, "5.seconds").map(Duration.parse)
      } yield (NewRelicConfig(apiKey, newRelicUri, maxMetricsPerRequest, maxPublishingDelay)))
      .orDie

  val fromEnvEULayer = fromEnvLayer.project(_.copy(newRelicURI = NewRelicUri.EU))

  private lazy val envApiKey               = "NEW_RELIC_API_KEY"
  private lazy val envMetricsUri           = "NEW_RELIC_URI"
  private lazy val envMaxMetricsPerRequest = "NEW_RELIC_MAX_METRICS_PER_REQUEST"
  private lazy val envMaxPublishingDelay   = "NEW_RELIC_MAX_PUBLISHING_DELAY"
}
