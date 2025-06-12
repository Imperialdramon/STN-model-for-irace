import csv
from collections import defaultdict

def encontrar_solutions_exclusivas(filepath):
    soluciones_info = defaultdict(list)
    soluciones_validas = set()

    # Primera pasada: agrupar todas las apariciones por solution code
    with open(filepath, 'r', encoding='utf-8') as file:
        reader = csv.DictReader(file, delimiter='\t')
        for i, row in enumerate(reader, start=2):  # Línea 2 por encabezado
            sol1 = row["Solution1"].strip()
            sol2 = row["Solution2"].strip()
            elite = row["Elite1"].strip()
            tipo = row["Type1"].strip()

            soluciones_info[sol1].append({
                "linea": i,
                "elite": elite,
                "type": tipo,
                "sol1": sol1,
                "sol2": sol2
            })

    # Ahora analizamos si ese solution code está solamente vinculado a sí mismo con etiquetas ELITE+START
    resultados = []

    for sol_code, filas in soluciones_info.items():
        # Solo nos interesan soluciones que tienen al menos una fila con ELITE y START y sol1 == sol2
        tiene_elite_start_valido = any(
            f["elite"] == "ELITE" and f["type"] == "START" and f["sol1"] == f["sol2"]
            for f in filas
        )

        if not tiene_elite_start_valido:
            continue

        # Ahora verificamos si existen filas con:
        # - elite o type distintos
        # - o sol2 distinto a sol1
        tiene_otros_diferentes = any(
            (f["elite"] != "ELITE" or f["type"] != "START") or f["sol1"] != f["sol2"]
            for f in filas
        )

        if not tiene_otros_diferentes:
            # Si todos son ELITE+START y solo se apuntan a sí mismos → válido
            primera = next(f for f in filas if f["elite"] == "ELITE" and f["type"] == "START")
            resultados.append({
                "linea": primera["linea"],
                "solution": sol_code,
                "elite": primera["elite"],
                "type": primera["type"]
            })

    # Mostrar resultados
    print("Soluciones exclusivas ELITE+START sin otras combinaciones:")
    for r in resultados:
        print(f"Línea {r['linea']}: Solution = {r['solution']} | Elite = {r['elite']} | Type = {r['type']}")


archivo = 'Examples/ACOTSP/E1/Data/STN-i-E1-L0.txt'
encontrar_solutions_exclusivas(archivo)

#TODO: BORRAR
