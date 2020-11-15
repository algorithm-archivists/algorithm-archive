
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors
from matplotlib.cm import ScalarMappable


def f(z):
    return z**2


def magnitude_shading(f_val):
    f_val_abs = np.abs(f_val)
    return 0.5 + 0.5 * (f_val_abs - np.floor(f_val_abs))


def gridlines(f_val, threshold):
    return (np.abs(np.sin(np.pi * np.real(f_val))) ** threshold
            * np.abs(np.sin(np.pi * np.imag(f_val))) ** threshold)


def color(f_val, threshold):
    hue = (np.pi - np.angle(f_val)) / (2.0 * np.pi)
    saturation = magnitude_shading(f_val)
    value = gridlines(f_val, threshold)

    # Currently we have a tuple of 2D-arrays (hue, saturation, value).
    # This makes it a 2D-array of tuples, which the conversion function requires.
    hsv = np.moveaxis((hue, saturation, value), 0, -1)
    return matplotlib.colors.hsv_to_rgb(hsv)


if __name__ == "__main__":
    # Create a new figure containing a single plot
    fig, axes = plt.subplots(1, 1)

    # Set the title for the plot
    axes.set_title("$f(x)=z^2$")

    # Create color bar
    cbar = fig.colorbar(
        ScalarMappable(matplotlib.colors.Normalize(0.0, 2.0 * np.pi), "hsv"),
        ax=axes,
        label="Phase Angle")

    # Set x and y labels
    axes.set_xlabel("$Re(z)$")
    axes.set_ylabel("$Im(z)$")
    
    # Set color bar tick locations and labels
    cbar.set_ticks([0.0, np.pi, 2.0 * np.pi])
    cbar.set_ticklabels(["$0.0$", "$\pi$", "$2\pi$"])

    # Hide x and y ticks
    for tick in axes.get_xticklines():
        tick.set_visible(False)

    for tick in axes.get_yticklines():
        tick.set_visible(False)

    # Create a 500x500 input grid
    coords = np.linspace(-2.0, 2.0, 500)
    z_real, z_imag = np.meshgrid(coords, coords)
    z = z_real + 1j * z_imag

    # Calculate function values
    f_val = f(z)

    # Map function values to colors
    colors = color(f_val, 0.1)

    # Plot the colors
    #   extent=(-2.0, 2.0, -2.0, 2.0) sets the x and y ranges
    #   origin="lower" places index (0,0) of the color array in the lower-left corner
    #   aspect="equal" ensures that the plot is square
    axes.imshow(
        colors,
        extent=(-2.0, 2.0, -2.0, 2.0),
        origin="lower",
        aspect="equal")

    # Save output
    fig.savefig("domain.png")
