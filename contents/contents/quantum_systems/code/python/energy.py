import numpy as np


def calculate_energy(wfc, H_k, H_r, dx):
    """Calculate the energy <Psi|H|Psi>."""
    # Creating momentum conjugate wavefunctions
    wfc_k = np.fft.fft(wfc)
    wfc_c = np.conj(wfc)

    # Finding the momentum and real-space energy terms
    energy_k = 0.5 * wfc_c * np.fft.ifft((H_k ** 2) * wfc_k)
    energy_r = wfc_c * H_r * wfc

    # Integrating over all space
    energy_final = sum(energy_k + energy_r).real

    return energy_final * dx
