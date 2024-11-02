from setuptools import setup

setup(
    name='SMEOihaneSaez_py',
    version='0.1',
    packages='SMEOihaneSaez_py',
    description='Este paquete contiene funciones basicas para el analisis de datos como pueden ser el calculo de la correlacion, covarianza, estandarizacion, normalizacion y discretizacion de las variables.',
    author='Oihane Saez Murgiondo',
    author_email='osaez004@ikasle.ehu.eus',
    url='https://github.com/oihanesaez/SMEOihaneSaez/SMEOihaneSaez_py',
    long_description=open('README.txt').read(),
    install_requires=[
     "pandas >= 2.2.3", 
     "matplotlib >= 3.8.0", 
     "numpy >=2.1.0"
  ],
)
