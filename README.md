Tableau ALC Reasoner
==

$ scala -cp bin Main '(isFailureOf SOME Pillar) AND 
(isFailureOf SOME Column) AND 
(isFailureOf ONLY (NOT Column OR NOT Pillar))'


