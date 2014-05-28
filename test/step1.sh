#  renames files in $re into $de
#  
#  sed -e "s/^.*\(.\)$/\1/"
#  exiftool -Title="This is the Title" 

ifs=$'\n'

re=$(cat files)


arre=()
let j=0;
for f in $re; do
   arre[j]=$f
   let j=$j+1
done

let i=1;

token_p=${arre[1]}
token=${arre[1]}

let j=0;

total=${#arre[*]}

armv=""
for (( j=0; j<=$(( $total-1 )); j=j+2 )); do

	token_p=$token
	token=${arre[j+1]}

   if  [ "$token_p" !=  "$token" ]; then
      run="gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=./step1/z"$i".pdf "$(echo $armv)  #|sed 1,2d|sed 's/^/"/'|sed 's/$/"/'|awk 1 ORS='   '
		echo $run  #|bash   
      #echo "exiftool -Title=\"\" ./step1/z"$i".pdf"	
		let i=$i+1
      armv=""
   fi


   armv="$armv\"${arre[j]}\" "
   
done

run="gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=./step1/z"$i".pdf "$(echo $armv)
echo $run  #|bash   
#echo "exiftool -Title=\"\" ./step1/z"$i".pdf"	

