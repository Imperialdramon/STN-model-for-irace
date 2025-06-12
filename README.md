# STN-model-for-irace

Crear buena documentación de estas funciones

#### Ejemplo de crear STN-i codificada
Rscript generate_STN-i_data.R --input=Examples/ACOTSP/E1/Data/STN-i-E1-L0.txt \
  --output=Examples/ACOTSP/E1/STNs \
  --output_file="E1_L0.RData" \
  --problem_type="min" \
  --network_name="E1_L0"

Rscript generate_STN-i_data.R --input=Examples/ACOTSP/E2/Data/STN-i-E2-L0.txt \
  --output=Examples/ACOTSP/E2/STNs \
  --output_file="E2_L0.RData" \
  --problem_type="min" \
  --network_name="E2_L0"


Rscript generate_STN-i_data.R --input=Examples/ACOTSP/Example1/Data/STN-i-L0-ex1.txt \
  --output=Examples/ACOTSP/Example1/STNs \
  --output_file="Example1_L0.RData" \
  --problem_type="min" \
  --network_name="Example1_L0"

Rscript generate_STN-i_data.R --input=Examples/ACOTSP/Example2/Data/STN-i-L0-ex2.txt \
  --output=Examples/ACOTSP/Example2/STNs \
  --output_file="Example2_L0.RData" \
  --problem_type="min" \
  --network_name="Example1_L0"

#### Ejemplo de plotear STN-i
Rscript plot_STN-i.R --input=Examples/ACOTSP/E1/STNs/E1_L0.RData \
  --output=Examples/ACOTSP/E1/Plots \
  --output_file="E1_L0.pdf" \
  --layout_type="fr" \
  --show_regular=FALSE \
  --size_factor=1 \
  --palette=1

Rscript plot_STN-i.R --input=Examples/ACOTSP/Example1/STNs/Example1_L0.RData \
  --output=Examples/ACOTSP/Example1/Plots \
  --output_file="Example1_L0.pdf" \
  --layout_type="fr" \
  --show_regular=FALSE \
  --size_factor=1 \
  --palette=1

Rscript plot_STN-i.R --input=Examples/ACOTSP/Example2/STNs/Example2_L0.RData \
  --output=Examples/ACOTSP/Example2/Plots \
  --output_file="Example2_L0.pdf" \
  --layout_type="fr" \
  --show_regular=FALSE \
  --size_factor=1 \
  --palette=1


#### Ejemplo de mergear STNs-i

Rscript generate_merged_STN-i_data.R --input=Examples/ACOTSP/E1-E2/Data \
  --output=Examples/ACOTSP/E1-E2/Merged \
  --output_file="merged_E1-E2-L0.RData" \
  --criteria="mean"

Rscript generate_merged_STN-i_data.R --input=Examples/ACOTSP/Example1-2/Data \
  --output=Examples/ACOTSP/Example1-2/Merged \
  --output_file="merged_ex1-2.RData" \
  --criteria="mean"

#### Ejemplo de plotear un merged STN-i

Rscript plot_merged_STN-i.R --input=Examples/ACOTSP/E1-E2/Merged/merged_E1-E2-L0.RData \
  --output=Examples/ACOTSP/E1-E2/Plots \
  --output_file="merged_E1-E2-L0.pdf" \
  --layout_type="fr" \
  --show_shared=TRUE \
  --show_regular=TRUE \
  --show_elite=TRUE \
  --size_factor=1 \
  --palette=1
