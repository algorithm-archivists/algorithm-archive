package main

import (
	"fmt"
	"github.com/runningwild/go-fftw/fftw"
	"math/cmplx"
)

func fft(x []complex128, inv bool) {
	y := fftw.NewArray(len(x))
	for i := 0; i < len(x); i++ {
		y.Set(i, x[i])
	}

	if inv {
		plan := fftw.NewPlan(y, y, fftw.Backward, fftw.Estimate)
		plan.Execute()
		plan.Destroy()
	} else {
		plan := fftw.NewPlan(y, y, fftw.Forward, fftw.Estimate)
		plan.Execute()
		plan.Destroy()
	}

	for i := 0; i < len(x); i++ {
		x[i] = y.At(i) / cmplx.Sqrt(complex(float64(len(x)), 0))
	}

}

func calculateEnergy(wfc, h_r, h_k []complex128, dx float64) float64 {
	wfc_k := make([]complex128, len(wfc))
	wfc_c := make([]complex128, len(wfc))

	copy(wfc_k, wfc)
	fft(wfc_k, false)

	for i := 0; i < len(wfc); i++ {
		wfc_c[i] = cmplx.Conj(wfc[i])
	}

	energy_r := make([]complex128, len(wfc))
	energy_k := make([]complex128, len(wfc))

	for i := 0; i < len(wfc); i++ {
		energy_k[i] = wfc_k[i] * h_k[i]
	}

	fft(energy_k, true)

	for i := 0; i < len(wfc); i++ {
		energy_k[i] *= wfc_c[i]
		energy_r[i] = wfc_c[i] * h_r[i] * wfc[i]
	}

	energyFinal := 0.0

	for i := 0; i < len(wfc); i++ {
		energy_final += real(energy_k[i] + energy_r[i])
	}

	return energy_final * dx
}
