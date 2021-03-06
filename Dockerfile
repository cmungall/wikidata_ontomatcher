FROM swipl:8.0.3
MAINTAINER Chris Mungall <cjmungall@lbl.gov>
RUN apt-get update && apt-get install -y  --no-install-recommends make ca-certificates curl

ADD ./prolog/ /tools/prolog
ADD ./bin/ /tools/bin
ADD ./utf8.pl /tools/
ADD ./install.pl /tools/
ADD ./pack.pl /tools/
RUN curl -L -s https://raw.githubusercontent.com/cmungall/sparqlprog/master/bin/pl2sparql > /tools/bin/pl2sparql
RUN chmod +x /tools/bin/pl2sparql
WORKDIR /tools
RUN swipl -l install.pl -g install_requirements,halt
RUN swipl -l install.pl -g "pack_install('file:///tools/'),halt"
ENV PATH "/tools/bin:$PATH"

EXPOSE ${PORT}
CMD  swipl -p library=prolog ./bin/pl2sparql -u sparqlprog_wikidata -u wikidata_ontomatcher -e -s wd

