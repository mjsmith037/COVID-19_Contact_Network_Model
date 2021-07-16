<div class="center" id="modeldesc">

# COVID Network Model
## Model structure
  <img class="modelimg" src="model_framework.png" width="600px"/>

Time steps are single, 24 hour days.

Disease states follow a typical compartmental model structure.
Individuals are all initially classed as susceptible, which is appropriate for a novel virus such
as SARS-CoV-2, where no preexisting immunity is expected.

  <img class="modelimg" src="model_subsets/infection.png" width="450px"/>

Susceptible individuals have the potential to be infected when they interact with infectious individuals.
At each timestep (i.e. on each day), each susceptible person is infected based on the number
and type (such as classmates or coworkers) of interactions they share with infectious individuals.
The probability of infection for each of these interactions depends on
the parameter \\(\beta_\textrm{type of interaction}\\).
Upon infection, individuals transition to an "exposed" class, where
they do not yet infect others, but will eventually progress to an infectious stage.

In practice, we divide the task of modeling infections into two parts: first, we
generate networks of contacts. We do this by sequentially adding layers of interactions
to an underlying network in which each individual is loosely connected to all other
individuals in the network. We use this initial layer to signify all of the small
interactions we have as a society that we will not otherwise be modeling explicitly.
Think of this layer as the risk of being infected by your delivery person, or
while shopping in the supermarket. This risk is generally low, but it's presence allows
for the possibility of the disease to spread even when we are completely locked down.
The strength of this transmission route is set by the parameter \\(\beta_b\\).

To this underlying layer, we add family structure (based on a distribution of household
sizes reported in census data) and school structure (where children of similar ages are
grouped into "reasonably" sized classrooms). We also add explicit work interactions (working
off of a distribution of business sizes) and a potential approach to loosening full lockdowns
in which a small group of families agrees to be "socially monogamous" with each other,
creating a sort of super-household unit.

Each of these layers has an associated \\(\beta\\) value, signifying the likelihood of
being infected in a given timestep by an interaction of that type. To simplify the
interpretation of these many \\(\beta\\) values, we adopt a convention whereby we set
a "baseline" infection risk to be that of contracting COVID-19 from an infectious
individual in your own household. We use the arbitrary number of 0.33 for this
value. This means that every timestep you have an infectious individual in your
household, there is a 33% chance you will be infected by spending a day with them. Put another way,
the *average* time before you get infected by your housemate is 3 days (though most
people get infected much earlier than that).

Each layer's associated \\(\beta\\) value is then treated as a multiplier of this value,
which is assumed to be the highest rate of transmission. So, more concretely,
setting a value of \\(0.5\\) for transmission in schools (i.e. \\(\beta_c\\)),
corresponds to a daily infection probability of &#8776; 17&percnt;, half of the 33&percnt; baseline.

  <img class="modelimg" src="model_subsets/latent.png" width="400px"/>

Individuals transition from the exposed class at rate \\(\sigma\\)
(which is equivalent to the inverse of the average duration of the latent period).
Technically, this corresponds to the probability of transitioning between classes
in a given timestep (i.e. day) and an individual's actual time in a given class
will vary.

When an individual moves out of the exposed class, they join one of two
infectious classes: infectious and symptomatic (Infectious<sub>symptomatic</sub>)
or infectious and asymptomatic (Infectious <sub>asymptomatic</sub>).
The proportion assigned to the Infectious<sub>symptomatic</sub>
class is described by parameter \\(\rho\\), with the proportion Infectious<sub>asymptomatic</sub>
being \\(1 - \rho\\).

  <img class="modelimg" src="model_subsets/recovery.png" width="400px"/>

  Infectious
<sub>asymptomatic</sub>
  individuals only recover from
infection (i.e. they do not die from SARS-CoV-2 infection).
The recovery rate is described by parameter \\(\gamma\\), which is
equivalent to the inverse of the average duration of infection.
A key assumption in this formulation of the model is that asymptomatic
and symptomatic individuals transmit SARS-CoV-2 at equal rates.
Because individuals in this model do not alter behavior after infection
(including symptomatic infection), we deemed this a reasonable simplification.

  In contrast, Infectious
<sub>symptomatic</sub>
  individuals either recover or die from infection.
The recovery rate is described by parameter \\(\gamma\\), as above.

  <img class="modelimg" src="model_subsets/death.png" width="400px">

Deaths occur at a baseline rate for non-vulnerable individuals, parameterized by
\\(\mu\\), which is the binomial probability of death for a non-vulnerable,
symptomatically infectious individual per time step.

Vulnerable individuals experience a prolonged recovery rate and an additional mortality rate.
The recovery rate is the baseline recovery rate \\(\gamma\\) scaled by a factor of 3, based on data from (
  <a href="https://elifesciences.org/articles/57309" target="_blank">Bar-On et al. 2020</a>
). The additional mortality rate is given by \\(\nu\\), representing an additional
mortality rate, additive to the baseline mortality rate, for vulnerable individuals.

## Parameter references

<div class="center">
  <table>
   <thead>
    <tr>
     <th> Parameter </th>
     <th> Definition </th>
     <th> Point value </th>
     <th> Range </th>
     <th> Citations </th>
    </tr>
   </thead>
  <tbody>
    <tr>
     <td> \[\beta_b\] </td>
     <td> Background transmission rate </td>
     <td> 1 / population size </td>
     <td> 0.01 - 100 / population size </td>
     <td>  </td>
    </tr>
    <tr>
     <td> \[\beta_0\] </td>
     <td> Baseline transmission rate (i.e. transmission rate among housemates) </td>
     <td> 0.33 </td>
     <td> - </td>
     <td>  </td>
    </tr>
    <tr>
     <td> \[\beta_x\] </td>
     <td> Transmission rate modifier for interaction type \\(x\\). Actual transmission rate for this interaction type
          is calculated by multiplying this number by \\(\beta_0\\)</td>
     <td> varies </td>
     <td> 0 - 1 </td>
     <td>  </td>
    </tr>
    <tr>
     <td> \[\sigma\] </td>
     <td> The inverse of the average latent period duration </td>
     <td> 1/3.7 </td>
     <td> 1/4 - 1/3.3 </td>
     <td> <a href="https://elifesciences.org/articles/57309" target="_blank">Bar-On et al. 2020</a> </td>
    </tr>
    <tr>
     <td> \[\rho\] </td>
     <td> Proportion of new infectious individuals that are symptomatic </td>
     <td> 0.25 </td>
     <td> 0.18 - 0.31 </td>
     <td> <a href="https://onlinelibrary-wiley-com.ezp3.lib.umn.edu/doi/epdf/10.1002/jmv.25990">Wang et al. 2020</a> </td>
    </tr>
    <tr>
     <td> \[\gamma\] </td>
     <td> The inverse of the average duration of the infectious period </td>
     <td> 1/3.5 </td>
     <td> 1/4 - 1/3.1 </td>
     <td> <a href="https://elifesciences.org/articles/57309" target="_blank">Bar-On et al. 2020</a> </td>
    </tr>
    <tr>
     <td> \[\mu\] </td>
     <td> Baseline mortality rate for symptomatic, non-vulnerable individuals </td>
     <td> 0.00001 </td>
     <td> 0 - 1 </td>
     <td>  </td>
    </tr>
    <tr>
     <td> \[\nu\] </td>
     <td> Additional mortality due to vulnerability </td>
     <td> 0.14 </td>
     <td> 0 - 1 </td>
     <td>  </td>
    </tr>
  </tbody>
  </table>
</div>

## Other data sources

  Household size distribution (
    <a href="www/data/HouseholdSizeData_CensusTableHH-4.csv" target="_blank">Table HH-4</a>
  ) and individual age distributions (
    <a href="https://data.census.gov/cedsci/table?q=household%20size&amp;hidePreview=true&amp;tid=ACSDP1Y2018.DP05" target="_blank">Table DP05</a>
  ) based on national census data.
Ages were randomly assigned except that families were constrained to have at least one member aged 15 years or older.
Business sizes were based on the distribution found
<a href="https://www.naics.com/business-lists/counts-by-company-size/" target="_blank">here</a>
  with workplaces with fewer than 5 and greater than 100 employees omitted.
Because workplace size was stochastically assigned, the actual size distribution could vary from this baseline.
Adults of working age (20-64) were assigned to places of employment, with some adults not working (according to
                            <a href="https://mn.gov/deed/data/current-econ-highlights/state-national-employment.jsp" target="_blank">an unemployment rate of 9.4% documented in Minnesota in May 2020</a>).

Vulnerability was assigned randomly to inviduals based on their age class, in proportion to their
                            <a href="http://www.ecie.com.ar/images/paginas/COVID-19/4MMWR-Severe_Outcomes_Among_Patients_with_Coronavirus_Disease_2019_COVID-19-United_States_February_12-March_16_2020.pdf" target="_blank">documented hospitalization rates.</a>
</div>
