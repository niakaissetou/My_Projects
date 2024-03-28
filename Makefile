##
## EPITECH PROJECT, 2023
## MAKEFILE
## File description:
## FORPOOL
##
BIN_NAME = wolfram-exe

BIN_PATH = $(shell stack path --local-install-root)/bin/$(BIN_NAME)

NAME = wolfram

RM	=	rm -f

all:
	stack build
	cp $(BIN_PATH) $(NAME)

clean:
	stack clean

fclean:	clean
		$(RM) $(NAME)

re:	fclean all

.PHONY: all clean fclean re
