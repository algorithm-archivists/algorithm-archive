#------------split_op.jl-------------------------------------------------------#
#
# Plotting: to plot individual timesteps, use gnuplot like so:
#               p "output00000.dat" u 1:2 w l
#               rep "output00000.dat" u 1:3 w l
#
#------------------------------------------------------------------------------#

using FFTW

struct Param
    xmax::Float64
    res::Int64
    dt::Float64
    timesteps::Int64
    dx::Float64
    x::Vector{Float64}
    dk::Float64
    k::Vector{Float64}
    im_time::Bool

    Param() = new(10.0, 512, 0.05, 1000, 2 * 10.0/512,
                  Vector{Float64}(-10.0 + 10.0/512 : 20.0/512 : 10.0),
                  pi / 10.0,
                  Vector{Float64}(vcat(0:512/2 - 1, -512/2 : -1) * pi/10.0),
                  false)
    Param(xmax::Float64, res::Int64, dt::Float64, timesteps::Int64,
          im_val::Bool) = new(
              xmax, res, dt, timesteps,
              2*xmax/res, Vector{Float64}(-xmax+xmax/res:2*xmax/res:xmax),
              pi/xmax, Vector{Float64}(vcat(0:res/2-1, -res/2:-1)*pi/(xmax)),
              im_val
          )
end

mutable struct Operators
    V::Vector{Complex{Float64}}
    R::Vector{Complex{Float64}}
    K::Vector{Complex{Float64}}
    wfc::Vector{Complex{Float64}}

    Operators(res) = new(zeros(res),
                         zeros(res),
                         zeros(res),
                         zeros(res))
end

# Function to initialize the wfc and potential
function init(par::Param, voffset::Float64, wfcoffset::Float64)
    opr = Operators(length(par.x))
    opr.V = 0.5 * (par.x .- voffset).^2
    opr.wfc = exp.(-(par.x .- wfcoffset).^2/2)
    if (par.im_time)
        opr.K = exp.(-0.5*par.k.^2*par.dt)
        opr.R = exp.(-0.5*opr.V*par.dt)
    else
        opr.K = exp.(-im*0.5*par.k.^2*par.dt)
        opr.R = exp.(-im*0.5*opr.V*par.dt)
    end

    return opr
end

# Function for the split-operator loop
function split_op!(par::Param, opr::Operators)

    for i = 1:par.timesteps
        # Half-step in real space
        opr.wfc = opr.wfc .* opr.R

        # fft to momentum space
        opr.wfc = fft(opr.wfc)

        # Full step in momentum space
        opr.wfc = opr.wfc .* opr.K

        # ifft back
        opr.wfc = ifft(opr.wfc)

        # final half-step in real space
        opr.wfc = opr.wfc .* opr.R

        # density for plotting and potential
        density = abs2.(opr.wfc)

        # renormalizing for imaginary time
        if (par.im_time)
            renorm_factor = sum(density) * par.dx

            for j = 1:length(opr.wfc)
                opr.wfc[j] /= sqrt(renorm_factor)
            end
        end

        # Outputting data to file. Plotting can also be done in a similar way
        # This is set to output exactly 100 files, no matter how many timesteps
        if ((i-1) % div(par.timesteps, 100) == 0)
            outfile = open("output" * string(lpad(string(i-1), 5, string(0)))
                                    * ".dat","w")

            # Outputting for gnuplot. Any plotter will do.
            for j = 1:length(density)
                write(outfile, string(par.x[j]) * "\t"
                               * string(density[j]) * "\t"
                               * string(real(opr.V[j])) * "\n")
            end

            close(outfile)
            println("Outputting step: ", i)
        end
    end
end

# We are calculating the energy to check <Psi|H|Psi>
function calculate_energy(par, opr)
    # Creating real, momentum, and conjugate wavefunctions
    wfc_r = opr.wfc
    wfc_k = fft(wfc_r)
    wfc_c = conj(wfc_r)

    # Finding the momentum and real-space energy terms
    energy_k = 0.5*wfc_c.*ifft((par.k.^2) .* wfc_k)
    energy_r = wfc_c.*opr.V .* wfc_r

    # Integrating over all space
    energy_final = 0
    for i = 1:length(energy_k)
        energy_final += real(energy_k[i] + energy_r[i])
    end

    return energy_final*par.dx
end

# main function
function main()
    par = Param(5.0, 256, 0.05, 100, true)

    # Starting wavefunction slightly offset so we can see it change
    opr = init(par, 0.0, -1.00)
    split_op!(par, opr)

    energy = calculate_energy(par, opr)
    println("Energy is: ", energy)
end

main()
