# Chaos Theory Graphics: Lorenz System

## Description

This Haskell project is based on graphing the Lorenz System in 2D/3D. The Lorenz System is based on a certain set of differential equations, which stand out for their chaotic behaviour and at the same time generate visually impressive graphs depending on the parameters introduced.

## **Screenshots**  <a name="screenshots"></a> 
Here are some screenshots:  

<p align="center">
  <img src="https://github.com/user-attachments/assets/54ed33ca-fc13-4477-8436-b4603a6dba46" width="45%">&nbsp;&nbsp;&nbsp;&nbsp;
  <img src="https://github.com/user-attachments/assets/8740f171-3160-410e-847d-79f267cf1566" width="45%">
  <br>
  <img src="https://github.com/user-attachments/assets/0a53d663-f270-47a0-97ae-4de637a94be6" width="45%">&nbsp;&nbsp;&nbsp;&nbsp;
  <img src="https://github.com/user-attachments/assets/86309b2c-94c6-4408-9990-278f5e03ce51" width="45%">
</p>

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
