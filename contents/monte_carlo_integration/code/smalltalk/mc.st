Object subclass: #PIFinder
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'AAA'

PIFinder>>finalize: succeeded of: total
	"Get that sweet PI."
	^4 * succeeded / (total)

Object subclass: #InCircleSampler
	instanceVariableNames: 'radius rand'
	classVariableNames: ''
	package: 'AAA'

InCircleSampler class>>new: newRadius
	|obj|
	obj := self new.
	obj radius: newRadius.
	^obj.
	
InCircleSampler>>initialize
	rand := Random new

InCircleSampler>>radius: newRadius
	radius := newRadius

InCircleSampler>>sample
	^{rand next * radius. rand next * radius}

Object subclass: #MonteCarlo
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'AAA'

MonteCarlo class>>solve: integrator with: sampler times: total then: solver
	"Monte Carlo solver."
	|successful|
	successful := 0.
	total timesRepeat: [ 
		(integrator check: (sampler sample)) ifTrue: [ 
			successful := successful + 1.
		]
	].
	^solver finalize: successful of: total

"Example"
|int sampler finder total pi precision|
int := InCircleIntegrator new: 1.
sampler := InCircleSampler new: 1.
finder := PIFinder new.

pi := MonteCarlo solve: int with: sampler times: 1000000 then: finder.
precision := pi - (Float pi) abs * 100 / (Float pi).

Transcript show: 'PI: '; show: pi asFloat; cr; show: '% error: '; show: precision abs; cr.
