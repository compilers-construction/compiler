test_dir=$(dirname $0)
for file in `ls $test_dir/cases` 
do 
	echo --------------------$file--------------------:
	test_expression=`utop tests/run_reader.ml $test_dir/cases/$file`
	if [ $? != 0 ]
	then 
		echo -n "Fail: Failure during reader execution"
		
	else
		res=`echo $test_expression | scheme -q`
		if [[ "$res" == *"#t"* ]]
		then
			echo -n "PASS: $res"
		else
			echo -n "FAIL: "
			echo -n $test_expression
		fi
	fi
	echo 
done
