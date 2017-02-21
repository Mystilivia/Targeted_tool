Cette application vise à faciliter le traitement des données issues d'analyses quantitatives du métabolisme ciblé. Elle a été conçue pour la plateforme [P2M2](https://www6.rennes.inra.fr/igepp_eng/About-IGEPP/Platforms/P2M2-Platform). Le tableau d'entrée doit être un fichier Excel avec trois feuilles de calculs :

  **(1) Les données (Echantillons x Variables) :** Contient les données brutes de chaque variable pour chaque échantillon.
  
  **(2) Les métadonnées des échantillons (Echantillons x Descripteurs) :** Descriptions des échantillons, ce tableau doit contenir au minimum une colonne "class" indiquant la nature des échantillons et optionnellement une colonne "batch" indiquant les séries analytiques. La colonne "class" indique la nature des échantillons : les standards externes "standard", les échantillons "sample" et optionnellement les contrôles qualités "qc" et blancs "blanc". La colonne "batch" indique la série à laquelle fait référence le(s) standard(s) externe(s), si elle est abscente, l'ensemble des données sera considéré comme faisant partie de la même série.
  
  **(3) Les métadonnées des variables (Variables x Descripteurs) :** Descriptions des variables, ce tableau doit contenir au minimum trois colonnes. Premièrement la liste des variables analysés (identiques au noms des colonnes dans (1)), deuxièmement une colonne "class" précisant le standard interne "SI" utilisé et troisièmement une colonne "conc" précisant les concentrations des standards externes.


*Pour voir un exemple, choisissez un des jeux de données de démonstration ci-dessous et cliquer sur télécharger.*
