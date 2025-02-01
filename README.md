# Chaos Theory Graphics: Lorenz System

## Description

This Haskell project is based on graphing the Lorenz System in 2D/3D. The Lorenz System is based on a certain set of differential equations, which stand out for their chaotic behaviour and at the same time generate visually impressive graphs depending on the parameters introduced.

## Features

- **Plotting**: Generates 2D/3D visualizations of the Lorenz attractor based on user-defined parameters.
- **Color Customization**: Allows customization of the colors of the graph.
- **Interactive Controls**: Incorporates interactive controls for manipulating the plotted graphs, such as zooming in and out, moving the camera and changing perspectives.

## Installation

To use this project, follow these steps:

1. Clone the repository: `git clone https://github.com/mqz05/Chaos-Theory-Graphics-Lorenz-System.git`
2. Navigate to the project directory: `cd ChaosTheoryGraphics`
5. Install required dependencies (if any): `stack build`

## Usage

- **Set Parameters**: Adjust the parameters of the Lorenz equations in the source code to modify the system's behavior and visualize different attractors.
- **Run the Application**: Execute the program with `stack run`.
- **Change the Perspective of the View**: Use the space bar to change perspectives of the view (from XY axis to XZ axis and vice versa)
- **Moving Camera**: Use the arrow keys to move the camera around the graph.
- **Zooming**: Use "+" and "-" zoom in and out.

## Acknowledgments

- Lorenz, Edward Norton (1963). "Deterministic nonperiodic flow". Journal of the Atmospheric Sciences. 20 (2): 130–141. doi: https://doi.org/10.1175/1520-0469(1963)020%3C0130:DNF%3E2.0.CO;2
- Built with [Haskell](https://www.haskell.org/) and [Graphics.Gloss](https://hackage.haskell.org/package/gloss).
