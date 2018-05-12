using Plots
pyplot()

# Module to hold all parameters for simulation
module Par
    const xmax = 10.0
    const res = 512
    const dt = 0.05
    const timesteps = 1000
    const dx = 2*xmax / res
    const x = -xmax+dx:dx:xmax
    const dk = pi / xmax
    const k = vcat(0:res/2-1, -res/2:-1)*dk
end

# Type to hold all operators
type Operators
    V::Vector{Complex{Float64}}
    PE::Vector{Complex{Float64}}
    KE::Vector{Complex{Float64}}
    wfc::Vector{Complex{Float64}}
end

# Function to initialize the wfc and potential
function init(voffset::Float64, wfcoffset::Float64)
    V = 0.5 * (Par.x - voffset).^2
    wfc = 3* exp.(-(Par.x-wfcoffset).^2/2)
    PE = exp.(-0.5*im*V*Par.dt)
    KE = exp.(-0.5*im*Par.k.^2*Par.dt)

    opr = Operators(V, PE, KE, wfc)
end

# Function for the split-operator loop
function split_op(opr::Operators)

    for i = 1:Par.timesteps
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
    opr = init(0.0, 1.0)
    split_op(opr)
end

main()
