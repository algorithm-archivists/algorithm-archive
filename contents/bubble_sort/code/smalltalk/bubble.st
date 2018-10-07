"Add this method to the SequenceableCollection class in the browser:"
SequenceableCollection>>bubbleSort
    "Bubble sort for a collection."
    | len swapper thisElem nextElem |
    len  := self size.
    1 to: len - 1 do:  [ :iteration |
        1 to: len - 1 do: [ :index |
            thisElem := self at: index.
            nextElem := self at: index + 1.
            (thisElem > nextElem) ifTrue: [ 
                self at: thisIndex + 1 put: thisElem.
                self at: thisIndex put: nextElem.
            ]
        ]
    ]

"Then run this anywhere in your code: "
#(4 3 2 1 6 5) bubbleSort "outputs:  #(1 2 3 4 5 6)"
