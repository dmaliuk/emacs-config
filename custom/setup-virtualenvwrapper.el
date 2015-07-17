(require 'virtualenvwrapper)
(venv-initialize-eshell)

;; venv locations (can be a list)
(setq venv-location '("/Users/dmaliuk/fall14/python/virtualenvs/convnet-stanford/"
                      "/Users/dmaliuk/fall14/python/virtualenvs/quantecon/"
                      "/Users/dmaliuk/fall14/python/lasagne_facial_tut/lasagne.ve/"
                      "/Users/dmaliuk/fall14/python/theano-tutorial/theano-venv/"
                      "/Users/dmaliuk/fall14/python/virtualenvs/apollo-caffe/"))

(provide 'setup-virtualenvwrapper)
