$CLASSPATH="../bin";
#$CLASSPATH="/tmp/dl-gre/build/classes";


$repetitions = 10;

$min_numNodes = 10;
$max_numNodes = 200;
$step_numNodes = 10;

$min_numPreds = 10;
$max_numPreds = 10;
$step_numPreds = 1;

$min_numRoles = 4;
$max_numRoles = 4;
$step_numRoles = 1;

$min_predDensity = 0.1;
$max_predDensity = 0.1;
$step_predDensity = 10;

$min_edgeDensity = 0.1;
$max_edgeDensity = 0.1;
$step_edgeDensity = 10;

#$mode = "positive";
$mode = "bisim";

for( $edgeDensity = $min_edgeDensity; $edgeDensity <= $max_edgeDensity; $edgeDensity += $step_edgeDensity ) {
  for( $predDensity = $min_predDensity; $predDensity <= $max_predDensity; $predDensity += $step_predDensity ) {
    for( $numRoles = $min_numRoles; $numRoles <= $max_numRoles; $numRoles += $step_numRoles ) {
      for( $numPreds = $min_numPreds; $numPreds <= $max_numPreds; $numPreds += $step_numPreds ) {
	for( $numNodes = $min_numNodes; $numNodes <= $max_numNodes; $numNodes += $step_numNodes ) {
	  $cmd = "java -cp $CLASSPATH:/Applications/eclipse/plugins/ch.epfl.lamp.sdt.compiler_2.7.0.14321-final_0/lib/scala-library.jar:../packages/jgrapht-jdk1.5.jar dlgre.IterateRandom $mode $repetitions 20 $numNodes $numPreds $numRoles $predDensity $edgeDensity";
	  $sum_runtime = 0;
	  $sum_passes = 0;
	  $sum_classes = 0;

	  print STDERR ".";

	  $output = `$cmd`;

	  if( $output =~ /Average runtime: (\S+)/ms ) {
	    print("$numNodes,$numPreds,$numRoles,$predDensity,$edgeDensity,$1\n");
	  }
	}
      }
    }
  }
}

print STDERR "\n";
