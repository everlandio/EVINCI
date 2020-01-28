#PBS -l select=1:ncpus=2
#PBS -l walltime=25:00:00

module load gcc
module load R

cd /mnt/nfs/home/EVER/EVINCI
R CMD BATCH --no-save --no-restore "--args $ARFF $fold $exec" script2.R ./logs/$ARFF$fold$exec.log
