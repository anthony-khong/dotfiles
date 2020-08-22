VIM-DOCKERNAME=anthony-khong/vim

build-nvim:
	docker build -f docker/vim/Dockerfile \
		-t $(VIM-DOCKERNAME):latest \
		docker/vim

nvim: build-nvim
	docker run --rm \
		-v $(PWD):/root/dotfiles \
		-w /root/dotfiles \
		-it $(VIM-DOCKERNAME) \
		/bin/bash
