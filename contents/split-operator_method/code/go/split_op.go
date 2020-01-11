package main

import (
	"bufio"
	"fmt"
	"github.com/runningwild/go-fftw/fftw"
	"math"
	"math/cmplx"
	"os"
	"strconv"
)

type params struct {
	xmax      float64
	res       int
	dt        float64
	timesteps int
	dx        float64
	x         []float64
	dk        float64
	k         []float64
	im_time   bool
}

type operators struct {
	size int
	v    []complex128
	pe   []complex128
	ke   []complex128
	wfc  []complex128
}

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

func initParams(xmax, dt float64, res, timesteps int, im bool) params {
	var par params
	par.xmax = xmax
	par.res = res
	par.dt = dt
	par.timesteps = timesteps
	par.dx = 2.0 * xmax / float64(res)
	par.x = make([]float64, res)
	par.dk = math.Pi / xmax
	par.k = make([]float64, res)
	par.im_time = im

	for i := 0; i < res; i++ {
		par.x[i] = xmax/float64(res) - xmax + float64(i)*(2.0*xmax/float64(res))
		if i < res/2 {
			par.k[i] = float64(i) * math.Pi / xmax
		} else {
			par.k[i] = float64(i-res) * math.Pi / xmax
		}
	}
	return par
}

func initOperators(par params, voffset, wfcoffset float64) operators {
	var opr operators
	opr.size = par.res
	opr.v = make([]complex128, par.res)
	opr.pe = make([]complex128, par.res)
	opr.ke = make([]complex128, par.res)
	opr.wfc = make([]complex128, par.res)

	for i := 0; i < par.res; i++ {
		opr.v[i] = 0.5 * cmplx.Pow(complex(par.x[i]-voffset, 0), 2)
		opr.wfc[i] = cmplx.Exp(-cmplx.Pow(complex(par.x[i]-wfcoffset, 0), 2) / 2)

		if par.im_time {
			opr.ke[i] = cmplx.Exp(-0.5 * complex(par.dt, 0) * cmplx.Pow(complex(par.k[i], 0), 2))
			opr.pe[i] = cmplx.Exp(-0.5 * complex(par.dt, 0) * opr.v[i])
		} else {
			opr.ke[i] = cmplx.Exp(complex(0, -0.5*par.dt*math.Pow(par.k[i], 2)))
			opr.pe[i] = cmplx.Exp(complex(-imag(opr.v[i]), -0.5*par.dt*real(opr.v[i])))
		}
	}
	return opr
}

func splitOp(par params, opr operators) {
	density := make([]float64, opr.size)

	for i := 0; i < par.timesteps; i++ {
		for j := 0; j < opr.size; j++ {
			opr.wfc[j] *= opr.pe[j]
		}

		fft(opr.wfc, false)

		for j := 0; j < opr.size; j++ {
			opr.wfc[j] *= opr.ke[j]
		}

		fft(opr.wfc, true)

		for j := 0; j < opr.size; j++ {
			opr.wfc[j] *= opr.pe[j]
		}

		for j := 0; j < opr.size; j++ {
			density[j] = math.Pow(cmplx.Abs(opr.wfc[j]), 2)
		}

		if par.im_time {
			sum := 0.0

			for j := 0; j < opr.size; j++ {
				sum += density[j]
			}

			sum *= par.dx

			for j := 0; j < opr.size; j++ {
				opr.wfc[j] /= complex(math.Sqrt(sum), 0)
			}
		}

		filename := "output" + strconv.Itoa(i) + ".dat"
		file, err := os.Create(filename)

		if err != nil {
			fmt.Println("Cant write: ", err)
			continue
		}
		defer file.Close()

		writer := bufio.NewWriter(file)

		fmt.Fprintf(writer, "%d\t%f\t%f\n", i, density[i], real(opr.v[i]))
	}
}

func calculateEnergy(par params, opr operators) float64 {
	wfc_r := make([]complex128, opr.size)
	wfc_k := make([]complex128, opr.size)
	wfc_c := make([]complex128, opr.size)

	copy(wfc_r, opr.wfc)
	copy(wfc_k, opr.wfc)
	fft(wfc_k, false)

	for i := 0; i < opr.size; i++ {
		wfc_c[i] = cmplx.Conj(wfc_r[i])
	}

	energy_r := make([]complex128, opr.size)
	energy_k := make([]complex128, opr.size)

	for i := 0; i < opr.size; i++ {
		energy_k[i] = wfc_k[i] * cmplx.Pow(complex(par.k[i], 0), 2)
	}

	fft(energy_k, true)

	for i := 0; i < opr.size; i++ {
		energy_k[i] *= 0.5 * wfc_c[i]
		energy_r[i] = wfc_c[i] * opr.v[i] * wfc_r[i]
	}

	energy_final := 0.0

	for i := 0; i < opr.size; i++ {
		energy_final += real(energy_k[i] + energy_r[i])
	}

	return energy_final * par.dx
}

func main() {
	par := initParams(5.0, 0.05, 256, 100, true)
	opr := initOperators(par, 0.0, -1.0)

	splitOp(par, opr)

	fmt.Printf("The energy is %f\n", calculateEnergy(par, opr))
}
