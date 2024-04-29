eshell-mode

(for "for " (p "i") " in " p " { " q " }")
(while "while { " p " } { " q " }")
(until "until { " p " } { " q " }")
(if "if { " p " } { " q " }")
(ife "if { " p " } { " p " } { " q " }")
(unl "unless { " p " } { " q " }")
(unle "unless { " p " } { " p " } { " q " }")

(mvngenerate "mvn archetype:generate -DgroupId=io.whatacold."
             (p "name" app)
             " -DartifactId="
             app
             " -DarchetypeArtifactId=maven-archetype-quickstart -DarchetypeVersion=1.4 -DinteractiveMode=false")

(mvnrun "mvn exec:java -Dexec.mainClass='io.whatacold." (p "app name") ".App'" q)
