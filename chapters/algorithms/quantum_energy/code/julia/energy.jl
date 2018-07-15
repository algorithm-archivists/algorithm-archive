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
