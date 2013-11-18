Tableau ALC Reasoner
==

$ scalac -feature -deprecation -cp bin -d bin src/ALC.scala
$ scalac -feature -deprecation -cp bin -d bin src/Parser.scala
$ scalac -feature -deprecation -cp bin -d bin src/Tableau.scala

$ scala -cp bin Main '(isFailureOf SOME Pillar) AND 
(isFailureOf SOME Column) AND 
(isFailureOf ONLY (NOT Column OR NOT Pillar))'

$ scala -cp bin Main 'person AND
(eats ONLY (plant OR dairy)) AND
(NOT person OR (eats SOME NOT plant))'


