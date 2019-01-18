#!/bin/bash

echo -e "Foo\nBar\nBaz" | docker run -i --rm pingpongmageddon pingpongmageddon -n Fjas
