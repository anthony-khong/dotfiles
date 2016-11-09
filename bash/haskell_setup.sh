curl -sSL https://get.haskellstack.org/ | sh
xcode-select --install
stack setup
stack install stylish-haskell
stack install ghc-mod
stack install random

rm ~/.stack/config.yaml
echo -e "templates:\n \tparams:\n \t\tauthor-name: Anthony Khong\n \t\tauthor-email: anthony.kusumo.khong@gmail.com\n \t\tcategory: Your Projects Category\n \t\tcopyright: 'Copyright: (c) 2016 Anthony Khong'\n \t\tgithub-username: anthony-khong\n" >> .stack/config.yaml
