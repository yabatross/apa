function existe {
	`cat ricachones.txt | grep -i $1 >/dev/null`
	if [ $? -eq 0 ]; then
		echo "$1: 1"
	else
		echo "$1: 0"
	fi
}


existe "united-states"
existe "United-States"
existe "Cambodia"
existe "England"
existe "Puerto-Rico"
existe "Canada"
existe "Germany"
existe "Outlying-US(Guam-USVI-etc)"
existe "India"
existe "Japan"
existe "Greece"
existe "South"
existe "China"
existe "Cuba"
existe "Iran"
existe "Honduras"
existe "Philippines"
existe "Italy"
existe "Poland"
existe "Jamaica"
existe "Vietnam"
existe "Mexico"
existe "Portugal"
existe "Ireland"
existe "France"
existe "Dominican-Republic"
existe "Laos"
existe "Ecuador"
existe "Taiwan"
existe "Haiti"
existe "Columbia"
existe "Hungary"
existe "Guatemala"
existe "Nicaragua"
existe "Scotland"
existe "Thailand"
existe "Yugoslavia"
existe "El-Salvador"
existe "Trinadad&Tobago"
existe "Peru"
existe "Hong"
existe "Holand-Netherlands"

