#!/bin/bash
#SBATCH --job-name="saxpy GPU"
#SBATCH --output=stdout.out
#SBATCH --partition=gpu
#SBATCH --ntasks=1
#SBATCH --mem=4G
#SBATCH --gpus=1
#SBATCH --time=15:00
#SBATCH --account=ulghpsc

module load EasyBuild/2022a
module load Clang/16.0.6-GCCcore-11.3.0-CUDA-11.7.0

export OMP_TARGET_OFFLOAD=MANDATORY

./build/saxpy_gpu
