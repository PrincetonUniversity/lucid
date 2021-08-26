# find p4tapp.py

if [ -z "${P4TAPP}" ]
then 
	# echo '$P4TAPP not set, searching for p4tapp.py...'
	# roots=("." ".." "../.." "../../..")

	for root in "." ".." "../.." "../../.." "../../../.." "../../../../.."
	do
		P4TAPP=`find ${root} -name "p4tapp.py"`
		if [ -z "${P4TAPP}" ]
		then
			:
		else
			# echo "found in: ${P4TAPP}"
			break
		fi
	done	
fi

if [ -z "${P4TAPP}" ]
then
	echo ""
	# echo "error: could not find p4tapp.py!"
	exit
fi
P4TAPP=`realpath ${P4TAPP}`
echo ${P4TAPP}