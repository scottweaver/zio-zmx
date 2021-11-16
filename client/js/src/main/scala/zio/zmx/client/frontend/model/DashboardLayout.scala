package zio.zmx.client.frontend.model

import zio.Chunk

object Layout {

  sealed trait Dashboard[+T]

  object Dashboard {
    case object Empty                                       extends Dashboard[Nothing]
    final case class Cell[+T](config: T)                    extends Dashboard[T]
    final case class HGroup[+T](elems: Chunk[Dashboard[T]]) extends Dashboard[T]
    final case class VGroup[+T](elems: Chunk[Dashboard[T]]) extends Dashboard[T]

    implicit class DashboardOps[T](self: Dashboard[T]) {

      /**
       * Combine two Dashboards placing them next to each other horizontally
       */
      def ||(other: Dashboard[T]): Dashboard[T] = self match {
        case Empty          => other
        case HGroup(elems1) =>
          other match {
            case HGroup(elems2) => HGroup(elems1 ++ elems2)
            case db             => HGroup(elems1 :+ db)
          }
        case el             =>
          other match {
            case HGroup(elems) => HGroup(Chunk(el) ++ elems)
            case db            => HGroup(Chunk(el, db))
          }
      }

      /**
       * Combine two Dashboards placing them on top of each other
       */
      def ^^(other: Dashboard[T]): Dashboard[T] = self match {
        case Empty          => other
        case VGroup(elems1) =>
          other match {
            case VGroup(elems2) => VGroup(elems1 ++ elems2)
            case db             => VGroup(elems1 :+ db)
          }
        case el             =>
          other match {
            case VGroup(elems) => VGroup(Chunk(el) ++ elems)
            case db            => VGroup(Chunk(el, db))
          }
      }

      /**
       * Extend a Dashboard by a single column
       */
      def addColumn(cfg: T): Dashboard[T] = {
        val cell = Cell(cfg)
        self match {
          case Empty           => cell
          case cell: Cell[T]   => HGroup(Chunk(cell, cell))
          case HGroup(configs) => HGroup(configs :+ cell)
          case v: VGroup[T]    => HGroup(Chunk(v, cell))
        }
      }

      /**
       * Extend a Dashboard with a single row
       */
      def addRow(cfg: T): Dashboard[T] = {
        val cell = Cell(cfg)
        self match {
          case Empty         => cell
          case c: Cell[T]    => VGroup(Chunk(c, cell))
          case h: HGroup[T]  => VGroup(Chunk(h, cell))
          case VGroup(elems) => VGroup(elems :+ cell)
        }
      }

      def optimize: Dashboard[T] = {

        def group(elems: Chunk[Dashboard[T]], isHorizontal: Boolean): Dashboard[T] = {
          val removeEmpty = elems.filter(_ != Empty)

          removeEmpty match {
            case Chunk.empty => Empty
            case Chunk(e)    => e
            case o           =>
              // Within each group we combine the chunks that are "flowing" in the direction
              // so that a HGroup(HGroup(a), HGroup(b)) would become a HGroup(a::b)
              val combined: Chunk[Dashboard[T]] =
                o.foldLeft[Chunk[Chunk[Dashboard[T]]]](Chunk.empty) { case (cur, d) =>
                  d match {
                    case HGroup(elems) if isHorizontal  => cur :+ elems
                    case VGroup(elems) if !isHorizontal => cur :+ elems
                    case o                              => cur :+ Chunk(o)
                  }
                }.flatten
              if (isHorizontal) HGroup(combined) else VGroup(combined)
          }
        }

        self match {
          case Empty         => Empty
          case c: Cell[T]    => c
          case HGroup(elems) =>
            group(elems.map(_.optimize), true)
          case VGroup(elems) =>
            group(elems.map(_.optimize), false)
        }
      }
    }
  }
}
