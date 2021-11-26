package zio.zmx.client.frontend.views

import zio.Chunk
import zio.metrics.MetricKey

import com.raquo.laminar.api.L._
import zio.zmx.client.frontend.state.Command
import zio.zmx.client.frontend.model.PanelConfig.DisplayConfig
import zio.zmx.client.frontend.state.AppState

object PanelConfigDialog {

  def render($cfg: Signal[DisplayConfig], id: String): HtmlElement =
    new PanelConfigDialogImpl($cfg, id).render()

  private class PanelConfigDialogImpl($cfg: Signal[DisplayConfig], dlgId: String) {

    private val curTitle: Var[String]                  = Var("")
    private val selectedMetrics: Var[Chunk[MetricKey]] = Var(Chunk.empty)

    private val metricSelected: Observer[MetricKey] = Observer[MetricKey] { key =>
      selectedMetrics.update(cur => if (!cur.contains(key)) cur :+ key else cur)
    }

    private val metricRemoved: Observer[MetricKey] = Observer[MetricKey] { key =>
      selectedMetrics.update(_.filter(!_.equals(key)))
    }

    private def metricsSelector(lbl: String, metrics: Signal[Chunk[MetricKey]]): HtmlElement = {
      val availableMetrics: Signal[Chunk[MetricKey]] => Signal[Chunk[MetricKey]] =
        _.combineWithFn[Chunk[MetricKey], Chunk[MetricKey]](selectedMetrics.signal) { case (known, selected) =>
          known.filter(k => !selected.contains(k))
        }

      MetricsSelector(lbl, metricSelected).render(availableMetrics(metrics))
    }

    def render(): HtmlElement =
      div(
        idAttr := dlgId,
        cls := "modal",
        child <-- $cfg.map { cfg =>
          div(
            cls := "modal-box max-w-full m-12 border-2 flex flex-col bg-accent-focus text-accent-content",
            div(
              cls := "border-b-2",
              span("Panel configuration")
            ),
            div(
              cls := "flex-grow",
              div(
                cls := "form-control",
                label(cls := "label", span(cls := "label-text", "Title")),
                input(
                  tpe := "text",
                  cls := "input input-primary input-bordered",
                  placeholder("Enter Diagram title"),
                  value := cfg.title,
                  onMountCallback { _ =>
                    curTitle.set(cfg.title)
                    selectedMetrics.set(cfg.metrics)
                  },
                  onInput.mapToValue --> curTitle
                )
              ),
              MetricsSelector("Configured metrics", metricRemoved).render(selectedMetrics.signal, true),
              metricsSelector("Available Counters", AppState.knownCounters),
              metricsSelector("Available Gauges", AppState.knownGauges),
              metricsSelector("Available Histograms", AppState.knownHistograms),
              metricsSelector("Available Summaries", AppState.knownSummaries),
              metricsSelector("Available Set Counts", AppState.knownSetCounts)
            ),
            div(
              cls := "modal-action",
              a(
                href := "#",
                cls := "btn btn-secondary",
                onClick.map(_ => cfg.title) --> curTitle,
                "Cancel"
              ),
              a(
                href := "#",
                cls := "btn btn-primary",
                onClick.map { _ =>
                  val curTimeseries = AppState.timeSeries.now().getOrElse(cfg.id, Map.empty)
                  val curMetrics    = selectedMetrics.now()

                  val newCfg = cfg.copy(
                    title = curTitle.now(),
                    metrics = curMetrics
                  )
                  Seq(
                    Command.UpdateDashboard(newCfg),
                    Command.ConfigureTimeseries(
                      cfg.id,
                      curTimeseries.filter { case (k, _) => curMetrics.contains(k.metric) }
                    )
                  )
                } --> Command.observerN,
                "Apply"
              )
            )
          )
        }
      )
  }
}
