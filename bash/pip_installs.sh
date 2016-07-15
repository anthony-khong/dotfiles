pip install --upgrade pip

# Machine Learning
pip install scikit-learn
pip install fancyimpute
pip install Theano
echo "[global]" >> .theanorc
echo "floatX = float32" >> .theanorc
echo "" >> .theanorc
echo "[nvcc]" >> .theanorc
echo "fastmath = True" >> .theanorc
conda install pydot
pip install Lasagne

# Others
pip install fuzzywuzzy
pip install ggplot
pip install jedi
pip install neovim
pip install pdbpp
pip install speedtest-cli
pip install youtube-dl

# Tensorflow
if [ "$(uname)" == "Darwin" ]; then
    pip install --upgrade https://storage.googleapis.com/tensorflow/mac/tensorflow-0.8.0-py2-none-any.whl
fi

# XGB
cd cmls
git clone --recursive https://github.com/dmlc/xgboost
if [ "$(uname)" == "Darwin" ]; then
    brew install gcc --without-multilib
    cp make/minimum.mk ./config.mk
fi
make -j4
