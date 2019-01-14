#!/bin/bash

echo -e "Foo\nBar\nBaz" | docker run -i --rm pingpongmageddon /root/generate_brackets fjas
