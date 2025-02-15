#!/usr/bin/env python3

# Script de conversion FLAC vers MP3
# Ce script parcourt un dossier spécifié (ou le dossier courant par défaut),
# convertit tous les fichiers FLAC en MP3 et enregistre les résultats dans un sous-dossier "mp3_files".
# Les conversions sont effectuées en parallèle pour accélérer le processus.
# Créé avec l'aide de ChatGPT.

import os
import subprocess
from concurrent.futures import ThreadPoolExecutor
import time
import sys

# Chemin alternatif vers l'exécutable ffmpeg si non trouvé dans le PATH
ffmpeg_path = r"C:\local\ffmpeg\bin\ffmpeg.exe"

# Qualité MP3 (2 = haute qualité, ajustable entre 0 et 9)
mp3_quality = 3

def get_ffmpeg_command():
    # Vérifier si ffmpeg est disponible dans le PATH
    try:
        subprocess.run(["ffmpeg", "-version"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, check=True)
        return "ffmpeg"
    except FileNotFoundError:
        # Si ffmpeg n'est pas dans le PATH, utiliser ffmpeg_path
        if os.path.isfile(ffmpeg_path):
            return ffmpeg_path
        else:
            print("Erreur : ffmpeg n'est pas disponible dans le PATH et n'a pas été trouvé à l'emplacement spécifié.")
            sys.exit(1)

def get_unique_output_dir(base_dir):
    output_dir = os.path.join(base_dir, "mp3_files")
    counter = 1
    while os.path.exists(output_dir):
        output_dir = os.path.join(base_dir, f"mp3_files_{counter}")
        counter += 1
    os.makedirs(output_dir)
    return output_dir

def convert_file(flac_file, mp3_file, ffmpeg_command):
    # Commande FFmpeg
    command = [
        ffmpeg_command,
        "-i", flac_file,         # Fichier source
        "-qscale:a", str(mp3_quality),  # Qualité MP3
        mp3_file                # Fichier de sortie
    ]

    print(f"Conversion de : {flac_file} en {mp3_file}...")
    try:
        subprocess.run(command, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        print(f"Terminé : {mp3_file}")
    except subprocess.CalledProcessError as e:
        print(f"Erreur lors de la conversion de {flac_file}: {e}")

def convert_flac_to_mp3(input_dir, ffmpeg_command):
    # Vérifier si le dossier existe
    if not os.path.isdir(input_dir):
        print(f"Le dossier spécifié n'existe pas : {input_dir}")
        sys.exit(1)

    # Créer un sous-répertoire unique pour les fichiers MP3
    output_dir = get_unique_output_dir(input_dir)

    # Liste des fichiers FLAC à convertir
    tasks = []
    for file in os.listdir(input_dir):
        if file.endswith(".flac"):
            flac_file = os.path.join(input_dir, file)
            mp3_file = os.path.join(output_dir, os.path.splitext(file)[0] + ".mp3")
            tasks.append((flac_file, mp3_file))

    # Utiliser ThreadPoolExecutor pour exécuter plusieurs conversions en parallèle
    with ThreadPoolExecutor() as executor:
        futures = [executor.submit(convert_file, flac, mp3, ffmpeg_command) for flac, mp3 in tasks]
        for future in futures:
            future.result()  # Attendre la fin de chaque tâche

    return len(tasks)

if __name__ == "__main__":
    if len(sys.argv) > 2:
        print("Utilisation : python script.py [<dossier_contenant_les_flac>]")
        sys.exit(1)

    input_directory = sys.argv[1] if len(sys.argv) == 2 else os.getcwd()
    ffmpeg_command = get_ffmpeg_command()
    start_time = time.time()
    total_files = convert_flac_to_mp3(input_directory, ffmpeg_command)
    end_time = time.time()
    elapsed_time = end_time - start_time
    print(f"Conversion terminée pour {total_files} fichiers FLAC en {elapsed_time:.2f} secondes.")
