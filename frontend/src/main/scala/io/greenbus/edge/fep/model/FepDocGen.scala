package io.greenbus.edge.fep.model

import io.greenbus.edge.data.doc.DocGen

object FepDocGen {

  def main(args: Array[String]): Unit = {
    DocGen.generate(FrontendSchema.all, System.out)
  }
}
