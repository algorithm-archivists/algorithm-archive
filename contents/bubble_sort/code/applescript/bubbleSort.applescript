on listToString(givenList)
	set generatedString to ""

	repeat with i from 1 to ((length of givenList) - 1)
		set generatedString to generatedString & item i of givenList & ", "
	end repeat
	set generatedString to generatedString & last item of givenList

	return generatedString
end listToString

on bubbleSort(givenList)
	repeat with i from 1 to length of givenList
		repeat with j from 2 to length of givenList
			if (item (j - 1) of givenList is greater than item j of givenList) then
				set temp to item (j - 1) of givenList
				set item (j - 1) of givenList to item j of givenList
				set item j of givenList to temp
			end if
		end repeat
	end repeat

	return givenList
end bubbleSort

set listBubble to {1, 2, 6, 4, 9, 54, 3, 2, 7, 15}
display alert listToString(listBubble)
set listBubble to bubbleSort(listBubble)
display alert listToString(listBubble)
