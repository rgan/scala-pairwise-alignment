

object SmithWatermanLocalAlignmentApp extends Application {
  
  override def main(args: Array[String]) {
      if (args.length < 2) {
        println("Usage: SmithWatermanLocalAlignment sequence1 sequence2")
        exit
      }
  }
}