FROM archlinux:base-20210620.0.26405
RUN mkdir -p /opt/myapp
WORKDIR /opt/myapp
COPY ./chess-server-fp-exe  /opt/myapp/chess-server-fp-exe
CMD ["/opt/myapp/chess-server-fp-exe"]