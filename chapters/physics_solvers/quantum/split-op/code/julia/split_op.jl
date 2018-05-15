using Plots
pyplot()

# struct to hold all parameters for simulation
struct Param
    xmax::Float64
    res::Int64
    dt::Float64
    timesteps::Int64
    dx::Float64
    x::Vector{Float64}
    dk::Float64
    k::Vector{Float64}

    Param() = new(10.0, 512, 0.05, 1000, 2*10.0 / 512,
                  Vector{Float64}(-10.0+10.0/512:20.0/512:10.0), pi / 10.0,
                  Vector{Float64}(vcat(0:512/2-1, -512/2:-1)*pi/10.0))
    Param(xmax::Float64, res::Int64, dt::Float64, timesteps::Int64) = new(
              xmax, res, dt, timesteps, 
              2*xmax/res, Vector{Float64}(-xmax+xmax/res:2*xmax/res:xmax),
              pi/xmax, Vector{Float64}(vcat(0:res/2-1, -res/2:-1)*pi/xmax)
          )
end

# struct to hold all operators
mutable struct Operators
    V::Vector{Complex{Float64}}
    PE::Vector{Complex{Float64}}
    KE::Vector{Complex{Float64}}
    wfc::Vector{Complex{Float64}}
end

# Function to initialize the wfc and potential
function init(par::Param, voffset::Float64, wfcoffset::Float64)
    V = 0.5 * (par.x - voffset).^2
    wfc = 3* exp.(-(par.x-wfcoffset).^2/2)
    PE = exp.(-0.5*im*V*par.dt)
    KE = exp.(-0.5*im*par.k.^2*par.dt)

    opr = Operators(V, PE, KE, wfc)
end

# Function for the split-operator loop
function split_op(par::Param, opr::Operators)

    for i = 1:par.timesteps
        # Half-step in real space
        opr.wfc = opr.wfc.*opr.PE

        # fft to phase space
        opr.wfc = fft(opr.wfc)

        # Full step in phase space
        opr.wfc = opr.wfc.*opr.KE

        # ifft back
        opr.wfc = ifft(opr.wfc)

        # final half-step in real space
        opr.wfc = opr.wfc.*opr.PE

        # plotting density and potential
        density = abs2.(opr.wfc)

        plot([density, real(opr.V)])
        savefig("density"*string(lpad(i, 5, 0))*".png")
        println(i)
    end
end

# main function
function main()
    par = Param(10.0, 512, 0.05, 1000)
    opr = init(par, 0.0, 1.0)
    split_op(par, opr)
end

main()
