
# openTelemac River Delta Model

## Overview
This repository contains the openTelemac model code for simulating the hydrodynamics and sediment transport processes in river deltas with the implementation of eco-geomorphic zones and DeLeeuw et al. (2020) entrainment relations. The model aims to introduce three different ways to model mud or cohesive s
provide insights into suspended sediment transport in coastal and deltaic regions, river-dominated delta formation, evolution, and the impact of various environmental factors.

openTelemac is an integrated suite of solvers for free-surface environmental flow, sediment transport, morphodynamics, waves, and water quality. It has been widely utilized in studies across different scales, from global ocean currents models to local high-resolution models. The code is developed and managed by Saint Venant Hydraulics Lab, under the EDF and École des Ponts ParisTech. The open-source approach allows anyone to take advantage of openTelemac and assess its performance.

## Model Solvers
- **Telemac2D**: This hydrodynamics solver is used to solve shallow water equations and incorporates a kappa-epsilon turbulence closure model. It is designed to accurately simulate the flow of water in two dimensions, which is essential for understanding the behavior of river deltas.
- **GAIA**: As the sediment transport solver, GAIA solves the Exner equation and handles morphodynamics. It is crucial for predicting sediment deposition and erosion patterns, which are key to modeling the evolution of river deltas.

## Features
- **Hydrodynamic Simulation**: Utilizes Telemac2D to model water flow dynamics.
- **Sediment Transport**: Employs GAIA for simulating erosion and deposition processes.
- **Delta Evolution**: Models the long-term evolution of river deltas under different scenarios.
- **Environmental Impact Assessment**: Assesses the impact of human activities and climate change on delta morphology.

## Installation
To use this model, please ensure you have openTelemac installed on your system. The installation guide can be found on the [openTelemac website](http://wiki.opentelemac.org/doku.php?id=installation_notes_2_beta).

1. Clone the repository:
```markdown
   git clone https://github.com/yourusername/openTelemac-river-delta-model.git
   ```
2. Navigate to the model directory:
   ```
   cd openTelemac-river-delta-model
   ```
3. Run the simulation using the provided scripts:
   ```
   ./run_simulation.sh
   ```

## Theoretical Background

### Shallow Water Equations in Telemac2D
The Telemac2D module within the openTelemac system is designed to solve the two-dimensional shallow water equations, which are a set of hyperbolic partial differential equations (PDEs) that describe fluid flow below a pressure surface in a body of water. These equations are derived from the conservation of mass and momentum (the Navier-Stokes equations) under the assumption that the horizontal length scales are much greater than the vertical length scale. Mathematically, the shallow water equations can be expressed as:

$$
\begin{aligned}
&\frac{\partial h}{\partial t} + \frac{\partial (hu)}{\partial x} + \frac{\partial (hv)}{\partial y} = 0, \\
&\frac{\partial (hu)}{\partial t} + \frac{\partial}{\partial x}(hu^2 + \frac{1}{2}gh^2) + \frac{\partial (huv)}{\partial y} = -gh\frac{\partial z_b}{\partial x} + S_{fx}, \\
&\frac{\partial (hv)}{\partial t} + \frac{\partial (huv)}{\partial x} + \frac{\partial}{\partial y}(hv^2 + \frac{1}{2}gh^2) = -gh\frac{\partial z_b}{\partial y} + S_{fy},
\end{aligned}
$$

where \( h \) is the water depth, \( u \) and \( v \) are the flow velocities in the \( x \) and \( y \) directions, respectively, \( g \) is the acceleration due to gravity, \( z_b \) is the bed elevation, and \( S_{fx} \), \( S_{fy} \) are the friction source terms in the \( x \) and \( y \) directions.

### Kappa-Epsilon Turbulence Closure in Telemac2D
The kappa-epsilon (k-ε) turbulence model is used in Telemac2D to simulate the effects of turbulence within the flow. This model is a two-equation model where the transport equations for the turbulent kinetic energy (k) and its dissipation rate (ε) are solved alongside the shallow water equations. The model equations are:

$$
\begin{aligned}
&\frac{\partial k}{\partial t} + u\frac{\partial k}{\partial x} + v\frac{\partial k}{\partial y} = P_k - \epsilon + \frac{\partial}{\partial x}\left(\nu_t\frac{\partial k}{\partial x}\right) + \frac{\partial}{\partial y}\left(\nu_t\frac{\partial k}{\partial y}\right), \\
&\frac{\partial \epsilon}{\partial t} + u\frac{\partial \epsilon}{\partial x} + v\frac{\partial \epsilon}{\partial y} = \frac{\epsilon}{k}(C_{1\epsilon}P_k - C_{2\epsilon}\epsilon) + \frac{\partial}{\partial x}\left(\nu_t\frac{\partial \epsilon}{\partial x}\right) + \frac{\partial}{\partial y}\left(\nu_t\frac{\partial \epsilon}{\partial y}\right),
\end{aligned}
$$

where \( P_k \) is the production of turbulent kinetic energy, \( \nu_t \) is the eddy viscosity, and \( C_{1\epsilon} \), \( C_{2\epsilon} \) are model constants.

### Exner Equation for Sediment Transport
The sediment transport within the model is governed by the Exner equation, which is a statement of conservation of mass for sediment particles. In this model, the Exner equation is solved in terms of the entrainment flux (\( E \)) and the depositional flux (\( D \)) of suspended sediment concentration. This formulation allows for a detailed representation of the sediment dynamics as it accounts for the rate at which sediment is incorporated into the flow (entrainment) and the rate at which it settles back onto the bed (deposition). The Exner equation in this context is given by:

$$
\frac{\partial z_b}{\partial t} = -\frac{1}{(1 - \lambda_p)}(E - D),
$$

where \( z_b \) is the bed elevation, \( \lambda_p \) is the bed porosity, \( E \) is the entrainment flux, and \( D \) is the depositional flux.

The entrainment flux \( E \) can be described as a function of the flow shear stress and the properties of the bed material, while the depositional flux \( D \) is related to the settling velocity of the sediment particles and the suspended sediment concentration. Together, these fluxes determine the net rate of change in the bed elevation due to sediment transport.

### Advection-Diffusion Equation for Suspended Sediment Transport
The transport of suspended sediment concentration in the model is described by the advection-diffusion equation, which accounts for the effects of sediment advection by the flow and diffusion due to turbulence. The equation also incorporates the depositional flux \( D \) and the entrainment flux \( E \), representing the rates of sediment settling and resuspension, respectively. The advection-diffusion equation is given by:

$$
\frac{\partial C}{\partial t} + u\frac{\partial C}{\partial x} + v\frac{\partial C}{\partial y} + w\frac{\partial C}{\partial z} = \frac{\partial}{\partial x}\left(\epsilon_x \frac{\partial C}{\partial x}\right) + \frac{\partial}{\partial y}\left(\epsilon_y \frac{\partial C}{\partial y}\right) + \frac{\partial}{\partial z}\left(\epsilon_z \frac{\partial C}{\partial z}\right) - D + E,
$$

where \( C \) is the suspended sediment concentration, \( u \), \( v \), and \( w \) are the flow velocities in the \( x \), \( y \), and \( z \) directions, respectively, and \( \epsilon_x \), \( \epsilon_y \), and \( \epsilon_z \) are the diffusion coefficients in the respective directions.

The depositional flux \( D \) is typically modeled as a function of the sediment settling velocity and the sediment concentration near the bed, while the entrainment flux \( E \) is related to the bed shear stress and the erodibility of the sediment bed. These fluxes are critical in determining the net sediment transport and the evolution of the river delta morphology.


### Depth-Averaged Advection-Diffusion Equation for Suspended Sediment Transport
The transport of suspended sediment concentration in the model is described by a depth-averaged advection-diffusion equation. This simplification reduces the complexity of the three-dimensional transport equation to two dimensions, making it more tractable for numerical simulation while still capturing the essential physics of sediment transport. The depth-averaged equation is given by:

$$
\frac{\partial (\bar{C} H)}{\partial t} + \nabla_h \cdot (\bar{C} H \mathbf{u}) = \nabla_h \cdot (k H \nabla_h \bar{C}) + E - D,
$$

where \( \bar{C} \) is the depth-averaged suspended sediment concentration, \( H \) is the total water depth, \( \mathbf{u} \) is the depth-averaged velocity vector, \( k \) is the depth-averaged diffusion coefficient, \( E \) represents the entrainment flux, and \( D \) represents the depositional flux.

In this equation, the term \( \nabla_h \cdot (\bar{C} H \mathbf{u}) \) represents the advection of sediment by the flow, \( \nabla_h \cdot (k H \nabla_h \bar{C}) \) represents the diffusion of sediment due to turbulence, and \( E - D \) accounts for the net change in sediment concentration due to entrainment and deposition processes.



## Contributing
Contributions to the openTelemac River Delta Model are welcome. Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct, and the process for submitting pull requests.

## License
This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

## Acknowledgments
- The openTelemac community for their support and guidance.
- All contributors who have helped shape this model.

## Contact
For any queries or collaboration opportunities, please contact the repository owner at `your.email@example.com`.

## References
- Please cite the following papers when using this model:
  - [Author et al., Year, "Title of the Paper", Journal Name, Volume(Issue), Pages.](link_to_paper)
  - [Author et al., Year, "Title of the Paper", Journal Name, Volume(Issue), Pages.](link_to_paper)

```

Make sure to replace placeholders like `yourusername`, `your.email@example.com`, and the links to papers with your actual GitHub username, contact information, and references. This should give users a clear understanding of the solvers used in your model and how they contribute to the simulation of river deltas.

Source: Conversation with Bing, 3/15/2024
(1) github.com. https://github.com/greysonmrx/Passarinhar/tree/3d88f691835b7a9cb008a9242c795ea06f63d7e3/README.md.
