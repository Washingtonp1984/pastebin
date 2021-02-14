#' Create a new paste
#'
#' @md
#' @param text of paste
#' @param name name/title of paste
#' @param format hint for syntax highlighting. Defaults to `text`. See
#'               [the detail page](https://pastebin.com/api#5) for more info.
#' @param impersonate if `TRUE` then `PASTEBIN_USER` and `PASTEBIN_PASSWORD` _must_ be set
#'        in order to generate a user key to be applied with the API key. Don't blame me,
#'        blame [pastebin](https://pastebin.com/api#8).
#' @param visibility one of `public`, `unlisted` or `private`. Defaults to `public`
#' @param expires either `n` for never or an abbreviated time expiration string in the form
#'                of a digit (the "number of") and a units character `m` for minute(s),
#'                `d` for day(s), `w` for week(s). Defaults to `n` (never). See
#'                [the detail page](https://pastebin.com/api#6) for more info.
#' @param pastebin_key pastebin API key
#' @note The maximum size a paste can be is 512 kilobytes (0.5 megabytes). Pro members are
#'       allowed to create pastes up to 10 megabytes.
#' @export
new_paste <- function(text, name=NULL, format="text", impersonate=FALSE,
                      visibility=c("public", "unlisted", "private"),
                      expires="n", pastebin_key=pastebin_api_key()) {

  expires <- gsub(" ", "", toupper(expires))

  visibility <- match.arg(visibility, c("public", "unlisted", "private"))
  visibility <- which(visibility == c("public", "unlisted", "private"))

  params <- list(api_dev_key=pastebin_key,
                 api_option="paste",
                 api_paste_code=text,
                 api_paste_name=name,
                 api_paste_format=format,
                 api_user_key="",
                 api_paste_expire_date=expires,
                 api_paste_private=visibility)

  if (impersonate) {

    httr::POST("https://pastebin.com/api/api_login.php",
               body=list(api_dev_key=pastebin_key,
                         api_user_name=Sys.getenv("PASTEBIN_USER"),
                         api_user_password=Sys.getenv("PASTEBIN_PASSWORD")),
               encode="form") -> u_res

    httr::stop_for_status(u_res)

    params$api_user_key <- httr::content(u_res, as="text", encoding="UTF-8")

  }

  httr::POST("https://pastebin.com/api/api_post.php", body=params, encode="form") -> res

  httr::stop_for_status(res)

  #EXTINF:-1 tvg-logo="https://i.imgur.com/yl6sJ46.jpg" group-title="Washington", Washington TV LISTA PREMIUM.
https://i.imgur.com/uKjOoWW.jpg

#EXTINF:-1 tvg-logo="https://i.imgur.com/yl6sJ46.jpg" group-title="Washington",☎️(11) 9 6026-9599
https://i.imgur.com/B5IXBfi.jpg
 

#EXTINF:-1 tvg-logo="https://i.imgur.com/yl6sJ46.jpg" group-title="Washington",☎️(19) 9 89992603
https://i.imgur.com/B5IXBfi.jpg


#EXTINF:-1 type="stream" tvg-id="RBI" tvg-logo="http://revenda.kudaplay.tv/tema/canais_logo/SD_RBI.png" group-title="Religioso", Gospel TV 
http://stmv2.euroti.com.br:1935/gmusic/gmusic/playlist.m3u8?PEDROJUNIORTUTORIAIS

#EXTINF:-1 tvg-logo="https://www.dci.com.br/wp-content/uploads/2021/01/BBB-21-logo.jpg" group-title="washinigton",BBB
https://streaming.encripted-encripted-encripted-encripted-encripted-encripted.fun/a/hls1/bbb21.m3u8?mu3zAQc9HC3GbwJq=xxiLs-5zg23gDjFi8RNviw&3U1G7qaTxrPbalZnEx=1612918776


#EXTINF:-1 tvg-logo="https://i.ibb.co/frYrFNM/globo.png" group-title="GLOBO", Globo sd
https://2d03lw.janjua.pw:8088/live/aovivotudo_globo/playlist.m3u8?vidictid=186676124876&id=3&pk=0c7411533dd1cc51532764237223ce6b22f04f5d5fba178eaa5755da3ed1eecd


#EXTINF:-1 tvg-logo="http://2.bp.blogspot.com/-yPkcv1F4kb8/UWDczhq7g9I/AAAAAAAB6wY/yCvmfO_TIhA/s1600/logotipo-viva-canal-da-globo-logo+(1).JPGg" roup-title="washington", Viva
https://2202ng.janjua.pw:8088/live/aovivotudo_viva/playlist.m3u8?vidictid=186671533118&id=77&pk=0c7411533dd1cc51532764237223ce6b22f04f5d5fba178eaa5755da3ed1eecd


#EXTINF:-1 tvg-logo="http://s2.glbimg.com/eO3jYDeim38C08X7WW8XHrOLhsE=/s3.glbimg.com/v1/AUTH_180b9dd048d9434295d27c4b6dadc248/media_kit/2b/d6/ecfa546cc63850824bb0b69141a5.png" group-title="ESPORTES", SPORTV
https://2204ng.janjua.pw:8088/live/aovivotudo_sportv/playlist.m3u8?vidictid=186677538364&id=33&pk=0c7411533dd1cc51532764237223ce6b22f04f5d5fba178eaa5755da3ed1eecd


#EXTINF:-1 tvg-logo="http://i.imgur.com/xmmiHls.png" group-title="ESPORTES",PREMIERE 
https://2205.janjua.pw:8088/live/aovivotudo_premier1/playlist.m3u8?vidictid=186674647381&id=79&pk=0c7411533dd1cc51532764237223ce6b22f04f5d5fba178eaa5755da3ed1eecd


#EXTINF:-1 tvg-logo="https://logos-download.com/wp-content/uploads/2016/10/TNT_logo.png"group-title="Filmes", TNT
https://2205.janjua.pw:8088/live/aovivotudo_premier1/playlist.m3u8?vidictid=186673542216&id=79&pk=0c7411533dd1cc51532764237223ce6b22f04f5d5fba178eaa5755da3ed1eecd


#EXTINF:-1 type="stream" tvg-id="Band" tvg-logo="http://treinandomentesmilionarias.com.br/wp-content/uploads/2015/07/logo-band.png?w=600" group-title="TV Aberta",Band
http://evpp.mm.uol.com.br:1935/geob_band/app/playlist.m3u8

#EXTINF:-1 tvg-logo="http://i.imgur.com/Jotugf0.png" group-title="CANAL ABERTO",SBT HD SP
https://5a1c76baf08c0.streamlock.net/tvsd2/smil:tvsd2_20042020.smil/chunklist_w521988676_b1000000.m3u8

#EXTINF:-1 tvg-logo="http://i.imgur.com/Jotugf0.png" group-title="CANAL ABERTO",SBT HD PE
https://evpp.mm.uol.com.br/ne10/ne10.smil/chunklist_w2019809400_b216000_sleng.m3u8


#EXTINF:-1 tvg-logo="http://listaiptvbrasil.com/logos/tvgazeta.png" group-title="CANAL ABERTO", TV GAZETA HD
http://stmv4.srvstm.com/wagner1168/wagner1168/playlist.m3u8?PEDROJUNIORTUTORIAIS

#EXTINF:-1 tvg-logo="https://cdn.mitvstatic.com/channels/record_m.jpg" group-title="RECORD",PT: TV RECORD
https://playplusbsa-lh.akamaihd.net/i/pp_bsa@377860/index_480_av-p.m3u8?sd=10&rebase=on

#EXTINF:-1 tvg-logo="https://cdn.mitvstatic.com/channels/br_rede-tv_m.png" group-title="REDE TV",RedeTV SP
https://59f1cbe63db89.streamlock.net:1443/tvpampa/_definst_/tvpampa/chunklist_w11344693.m3u8

#EXTINF:-1 tvg-logo="http://i.imgur.com/P0XMqwI.jpg" group-title="ABERTAS-NOTICIAS",RECORD NEWS
https://playplusnews-lh.akamaihd.net/i/pp_nws@377849/master.m3u8


#EXTINF:-1 tvg-logo="https://i.imgur.com/XUHf4SF.jpg group-title="ABERTAS-NOTICIAS", Manchete
https://srv5.zcast.com.br/tvmanchete/tvmanchete/chunklist_w50837556.m3u8


#EXTINF:-1 tvg-logo="https://i.ibb.co/CvvkxnR/tvcultura.png" group-title="Cultura", TV CULTURA HD
http://wowza4.catve.com.br:1935/live/livestream/chunklist_w1982702716.m3u8


#EXTINF:-1 tvg-logo="https://static.tvtropes.org/pmwiki/pub/images/117451l.jpg"Anime", Anime TV
https://stmv1.srvif.com/animetv/animetv/chunklist_w373244508.m3u8

#EXTINF:-1 type="stream" tvg-id="Disney" tvg-logo="https://2.bp.blogspot.com/-zcyQfQ7xiaw/WSaEod7H1GI/AAAAAAAA6AA/j84wbzvK5IcLFOz4l6evlGiD_Nn1nG5uQCLcB/s1600/2015_Disney_Channel_logo.svg.png" group-title="Infantil",PLAY KIDS
http://playplusdlab-lh.akamaihd.net/i/pp_pkids@377825/index_720_av-p.m3u8?PEDROJUNIORTUTORIAIS

#EXTINF:-1 type="stream" tvg-id="Disney" tvg-logo="https://2.bp.blogspot.com/-zcyQfQ7xiaw/WSaEod7H1GI/AAAAAAAA6AA/j84wbzvK5IcLFOz4l6evlGiD_Nn1nG5uQCLcB/s1600/2015_Disney_Channel_logo.svg.png" group-title="Infantil",Disney PLAY KIDS HD
http://playplusdlab-lh.akamaihd.net/i/pp_pkids@377825/index_720_av-p.m3u8?PEDROJUNIORTUTORIAIS

#EXTINF:-1 type="stream" tvg-id="Disney" tvg-logo="https://2.bp.blogspot.com/-zcyQfQ7xiaw/WSaEod7H1GI/AAAAAAAA6AA/j84wbzvK5IcLFOz4l6evlGiD_Nn1nG5uQCLcB/s1600/2015_Disney_Channel_logo.svg.png" group-title="Infantil",Disney XD
http://playplusdisney-lh.akamaihd.net/i/pp_dsneyxd@376337/index_360_av-b.m3u8?PEDROJUNIORTUTORIAIS

#EXTINF:-1 type="stream" tvg-id="Disney" tvg-logo="https://2.bp.blogspot.com/-zcyQfQ7xiaw/WSaEod7H1GI/AAAAAAAA6AA/j84wbzvK5IcLFOz4l6evlGiD_Nn1nG5uQCLcB/s1600/2015_Disney_Channel_logo.svg.png" group-title="Infantil",Disney JÚNIOR 
http://playplusdisney-lh.akamaihd.net/i/pp_dsneyjr@376337/index_360_av-b.m3u8?PEDROJUNIORTUTORIAIS

#EXTINF:-1 type="stream" tvg-id="Disney" tvg-logo="https://2.bp.blogspot.com/-zcyQfQ7xiaw/WSaEod7H1GI/AAAAAAAA6AA/j84wbzvK5IcLFOz4l6evlGiD_Nn1nG5uQCLcB/s1600/2015_Disney_Channel_logo.svg.png" group-title="Infantil",Disney CHANNEL 
http://playplusdisney-lh.akamaihd.net/i/pp_dsneych@376337/index_360_av-b.m3u8?PEDROJUNIORTUTORIAIS

#EXTINF:-1 tvg-logo="https://listaiptv.gratis/logos/imagens/kuriakos2bkids.png" group-title="Infantil", Kuriakos Kids ABN
https://moiptvhls-i.akamaihd.net/hls/live/652318/secure/master.m3u8?ANTONYGTV

#EXTINF:-1 tvg-logo="https://listaiptv.gratis/logos/imagens/kuriakos2bkids.png" group-title="Infantil", retrô cartoon
http://stmv1.srvif.com:1935/retrotv/retrotv/playlist.m3u8?PEDROJUNIORTUTORIAIS

#EXTINF:-1 tvg-logo="https://listaiptv.gratis/logos/imagens/smilinguido.png" group-title="Infantil", Gospel Cartoon
http://stmv1.srvstm.com:1935/gospelcartoon2/gospelcartoon2/playlist.m3u8?PEDROJUNIORTUTORIAIS

#EXTINF:-1 tvg-logo="https://listaiptv.gratis/logos/imagens/kuriakos2bkids.png" group-title="Infantil", Kuriakos Kids
http://c2.manasat.com:1935/kkids/kkids2/TeamBlue.m3u8


#EXTINF:-1 type="stream" tvg-id="Disney" tvg-logo="https://2.bp.blogspot.com/-zcyQfQ7xiaw/WSaEod7H1GI/AAAAAAAA6AA/j84wbzvK5IcLFOz4l6evlGiD_Nn1nG5uQCLcB/s1600/2015_Disney_Channel_logo.svg.png" group-title="Infantil",Disney Kids
http://www.toonxtv.net/hls/loco_ch/stream.m3u8
MTV

#EXTINF:-1 tvg-logo="https://i.imgur.com/yl6sJ46.jpg",Recon Sat Kids 
https://stmv1.srvif.com/reconsatkids/reconsatkids/chunklist_w913607849.m3u8


#EXTINF:-1 tvg-logo="https://i.imgur.com/8IyVTYj.jpg"group-title="Washington", TV On
https://livefocamundo.com:8081/8668/tracks-v1a1/mono.m3u8

#EXTINF:-1 tvg-logo="https://www.cxradio.com.br/img/Radio/Logo/3501f00c2cb9b00d3acbe312cf30a9f8.png" group-title="Washington", Play Funk
https://ssl1.transmissaodigital.com:20081/live

#EXTINF:-1 tvg-logo="https://br.vazlon.com/static/pics/2014/04/01/Grupo-de-pagode-20140401132716.jpg" group-title="Washington", Rádio Pagode
https://sc1s.cdn.upx.com:9061/stream

#EXTINF:-1 tvg-logo="http://cdn-radiotime-logos.tunein.com/s229696g.png" group-title="Washington", Rádio Reggae
https://casthttps.suaradionanet.net/13805/stream

#EXTINF:-1 tvg-logo="https://www.bauermedia.no/wp-content/uploads/2017/04/RadioRock-RR.png" group-title="Washington",Rádio Rock
https://f12.fabricahost.com.br/89aradiorocksp?f=1610224856N30cefecd2d5a3e44ca2ba6d5b98ddc06

#EXTINF:-1 tvg-logo="https://c1.staticflickr.com/5/4062/4625333036_97fe9f59e3_b.jpg" group-title="Washington", Rádio infantil
http://server07.srvsh.com.br:7570/;.?type=.flv

#EXTINF:-1 tvg-logo="https://www.kinderarztpraxis-babilas.de/img/leistungen/vorsorgeuntersuchung.jpg" group-title="Washington",Música p/ Ninar
http://stm18.xcast.com.br:7550/stream?type=.flv

#EXTINF:-1 tvg-logo="https://www.acheradios.com.br/media/imagens/radio-so-forro_crop_200x200.jpg" group-title="Washington", Rádio Só forró
http://centova10.ipstm.net:9318/stream

#EXTINF:-1 tvg-logo="http://minutoligado.com.br/wp-content/uploads/2013/12/jovem-pan.jpg" group-title="Washington",Jovem Pan MPB
https://sc1s.cdn.upx.com:8034/stream/1/

#EXTINF:-1 tvg-logo="https://i.ytimg.com/vi/lpGmIR7DlFs/hqdefault.jpg" group-title="Washington", Forró das antigas
http://node-32.zeno.fm/42s999qq3qruv?rj-ttl=5&rj-tok=AAABdujItCoARQA3kaWl6kjnVA

#EXTINF:-1 tvg-logo="https://i1.sndcdn.com/artworks-000056761947-o2t9nk-t500x500.jpg" group-title="Música", TOP Sertanejos
https://live.hunter.fm/sertanejo_high?origin=/country

#EXTINF:-1 tvg-logo="https://static.tuneyou.com/images/logos/500_500/34/13934/GospelFM89.3_9.png" group-title="Washington", Gospel FM 
https://player-ssl.kshost.com.br:9294/stream/1/

#EXTINF:-1 tvg-logo="https://studiosol-a.akamaihd.net/tb/1935x978/palcomp3-playlists/d2e94ed9-21c9-4f5e-a141-533f399897f2.jpg" group-title="Música", Sertanejo raiz
https://svrstream1.svreua.com/stream.php?porta=8172


#EXTINF:-1 tvg-logo="http://www.pluscomunicacao.com.br/wp-content/uploads/2015/07/logo-radio-cbn.jpg" group-title="washington",CBN SP
https://medias.sgr.globo.com/hls/vCBNSP/vCBNSP.m3u8


#EXTINF:-1 tvg-logo="http://i.imgur.com/ClaFSFI.png" group-title="FILMES/SERIES", Cine 24 hrs


#EXTINF:-1 tvg-logo="https://i.imgur.com/VLrlPva.jpg" group-title="cana", Cine patrocínio
http://painelvj.com.br/tvaguaboa2/tvaguaboa2.sdp/chunklist.m3u8

#EXTINF:-1 tvg-logo="https://i.imgur.com/yl6sJ46.jpg" group-title="Filmes", Washington filmes
https://streaming03.zas.media/canal24horas/canal24horas/chunklist_w440228248.m3u8

#EXTINF:-1 tvg-logo="http://i.imgur.com/ClaFSFI.png" group-title="FILMES/SERIES", Mega cine
https://stmv1.srvif.com/reconsatcine/reconsatcine/chunklist_w2082538825.m3u8

#EXTINF:-1 tvg-logo="http://i.imgur.com/ClaFSFI.png" group-title="FILMES/SERIES", Cine Turbo 24h
http://video01.kshost.com.br/fabio8255/fabio8255/chunklist_w207803064.m3u8

#EXTINF:-1 tvg-logo="https://i.imgur.com/dmen7IJ.jpg" group-title="FILMES/SERIES", CIne patrocínio 2
https://livefocamundo.com:8081/8588/index.m3u8

#EXTINF:-1 tvg-logo="http://i.imgur.com/ClaFSFI.png" group-title="FILMES/SERIES", Washington
https://streaming03.zas.media/canal24horas/canal24horas/chunklist_w1849114532.m3u8

#EXTINF:-1 tvg-logo="https://i.postimg.cc/L61CJkFk/images.jpg" group-title="FLMES", Washington filmes 01
https://livefocamundo.com:8081/8588/index.m3u8

#EXTINF:-1 tvg-logo="http://www.portalaltonia.com.br/files/2012/03/TV-MAIS.jpg" group-title="FLMES", TV Mais Filmes
https://livefocamundo.com:8081/8716/tracks-v1a1/mono.m3u8

#EXTINF:-1 tvg-logo="http://www.cxtv.com.br/img/Tvs/Logo/d586d6cee28415b8aadc9f9eb3887d9d.png" group-title="FLMES", TV Natal filmes
http://tvnatal.ddns.net:1935/vmix/myStream/chunklist_w408358270.m3u8

#EXTINF:-1 tvg-logo="http://i.imgur.com/ClaFSFI.png" group-title="FILMES/SERIES", Washington TV series
https://stmv4.srvstm.com/tvserie/tvserie/playlist.m3u8

#EXTINF:-1 type="stream" tvg-id="National Geographic" tvg-logo="http://revenda.kudaplay.tv/tema/canais_logo/SD_NAT_GEO.png" group-title="Documentario", National Geographic
https://streaming.encripted-encripted-encripted-encripted-encripted-encripted.fun/a/hls1/natgeo.m3u8?mu3zAQc9HC3GbwJq=4cIBZ_a5PuXV7YxmUuRVUw&3U1G7qaTxrPbalZnEx=1610592610

#EXTINF:-1 tvg-logo="https://i.ibb.co/8x3TjWH/discoverychannelb.png" group-title="Discovery",Discovery Channel
https://streaming.encripted-encripted-encripted-encripted-encripted-encripted.fun/a/hls1/discovery.m3u8?mu3zAQc9HC3GbwJq=MKJSZwnAywm0BTUbUAS_Ig&3U1G7qaTxrPbalZnEx=1610592831

#EXTINF:-1 type="stream" tvg-id="EI MAXX 2" tvg-logo="http://revenda.kudaplay.tv/tema/canais_logo/HD_ESPORTE_INTERATIVO_2.png" group-title="Esporte", Esporte interativo 
http://playplusespn-lh.akamaihd.net/i/pp_espnext@374460/index_360_av-b.m3u8?IMDSFULL

#EXTINF:-1 tvg-logo="https://i.imgur.com/HZVZclG.png" group-title="ESPN", Ufc Combate
https://2204ng.janjua.pw:8088/live/aovivotudo_combate/playlist.m3u8?vidictid=186485004079&id=17&pk=f9ff0a04bcc9a038d5918a88ac651c6c17c6a0d74ba8951fbe7f8a5bda7d83fa


#EXTINF:-1 type="stream" tvg-id="EI MAXX 2" tvg-logo="http://revenda.kudaplay.tv/tema/canais_logo/HD_ESPORTE_INTERATIVO_2.png" group-title="Esporte", Esporte interativo HD
http://playplusespn-lh.akamaihd.net/i/pp_espnbra@374460/index_720_av-p.m3u8?IMDSFULL

#EXTINF:-1 type="stream" tvg-id="EI MAXX 2" tvg-logo="http://revenda.kudaplay.tv/tema/canais_logo/HD_ESPORTE_INTERATIVO_2.png" group-title="Esporte", Esporte interativo HD Extra
http://playplusespn-lh.akamaihd.net/i/pp_espnext@374460/index_720_av-p.m3u8?IMDSFULL

#EXTINF:-1 tvg-logo="https://i.ibb.co/b3V4fX8/espn.png" group-title="ESPN",El Maxx
https://5cf4a2c2512a2.streamlock.net/dgrau/dgrau/playlist.m3u8?IMDSFULL

#EXTINF:-1 tvg-logo="http://i.imgur.com/pllt5rw.png" group-title="ESPORTES",ALL SPORTS TV
https://5cf4a2c2512a2.streamlock.net/dgrau/dgrau/chunklist.m3u8

#EXTINF:-1 tvg-logo="http://i.imgur.com/pllt5rw.png" group-title="ESPORTES", Combat go
https://dai.google.com/linear/hls/event/gaDje967RoSDD5llid2OBA/master.m3u8

#EXTINF:-1 tvg-logo="http://i.imgur.com/pllt5rw.png" group-title="ESPORTES",SPORTS Online
https://stmv1.srvstm.com/josehenrique1615/josehenrique1615/chunklist_w304599291.m3u8

#EXTINF:-1 type="stream" tvg-id="EI MAXX 2" tvg-logo="https://www.chapintv.com/wp-content/uploads/2020/10/Combate.jpg" group-title="Esporte",Combate BOXE
https://nrpus.bozztv.com/36bay2/gusa-tvsboxing/index.m3u8

#EXTINF:-1 type="stream" tvg-id="EI MAXX 2" tvg-logo="https://www.chapintv.com/wp-content/uploads/2020/10/Combate.jpg" group-title="Esporte", UFC Rus
http://live-ng-01.more.tv/hls/UFC/master.m3u8

#EXTINF:-1 type="stream" tvg-id="EI MAXX 2" tvg-logo="http://revenda.kudaplay.tv/tema/canais_logo/HD_ESPORTE_INTERATIVO_2.png" group-title="Esporte", Esporte interativo Plus
https://us-b4-p-e-ft6.cdn.mdstrm.com/live-stream/5bfc4171cb01650d5908a91a/playout/master.m3u8

#EXTINF:-1 tvg-logo="http://i.imgur.com/xmmiHls.png" group-title="ESPORTES",PREMIERE CLUBES JUVENTUS TV
https://juvetv-vh.akamaihd.net/i/juventus-free/gen_JTV_DEF-ENG-ALL_3e99-16_9_,iphone,.mp4.csmil/master.m3u8

#EXTINF:-1 tvg-logo="http://i.imgur.com/xmmiHls.png" group-title="ESPORTES",PREMIERE 6
https://2d03lw.janjua.pw:8088/live/aovivotudo_premier1/playlist.m3u8?vidictid=186458437248&id=79&pk=4b41d93b066201cf5cc5f0689308ee81b736580dc9348f57a383bb5a3dc5a6f0

#EXTINF:-1 tvg-logo="http://s2.glbimg.com/eO3jYDeim38C08X7WW8XHrOLhsE=/s3.glbimg.com/v1/AUTH_180b9dd048d9434295d27c4b6dadc248/media_kit/2b/d6/ecfa546cc63850824bb0b69141a5.png" group-title="ESPORTES", SPORTV 2
https://2202ng.janjua.pw:8088/live/aovivotudo_sportv2/playlist.m3u8?vidictid=186364122884&pk=78cd7f4556b169c2599a3d83889d2fde6d177bb28301ed40383edeae3f60ae7f&id=35


#EXTINF:-1 tvg-logo="https://img.redbull.com/image/upload/f_auto,q_auto/legacy/rbx00264/406/related-content/global/rtv_logo_single_pix_hor_rgb_black_bg-2.png" group-title="ESPORTES", REDBULL TV
https://rbmn-live.akamaized.net/hls/live/590964/BoRB-AT/master_3360.m3u8

#EXTINF:-1 tvg-logo="https://lut.im/YsRgTjnDOO/E4nuHDIFHqY6mBmY.png" group-title="WEB TV", VIVA TV WEB
http://video01.kshost.com.br/cypriano46326/cypriano46326/playlist.m3u8

#EXTINF:-1 tvg-logo="https://i.pinimg.com/originals/5a/d1/48/5ad1486b9aa62c4603e866765a340881.png" group-title="CANAL ABERTO",SUMMERTV
http://tv01.logicahost.com.br:1935/summertv/summertv/chunklist_w1820117447.m3u8


#EXTINF:-1 tvg-logo="https://i.imgur.com/vR2sSns.jpg" group-title="Música", TV TOP RÁDIO SERTANEJAS
https://stream.vagalume.fm/hls/1499715905423293/aac.m3u8

#EXTINF:-1 tvg-logo="https://i1.wp.com/klapptre.is/wp-content/uploads/2014/05/ISTV-logo.jpg",ISTV
https://stmv1.voxtvhd.com.br/istv/istv/chunklist_w899757443.m3u8

#EXTINF:-1 tvg-logo="https://2.bp.blogspot.com/-Aod38-uycMo/XYK0XtCfIhI/AAAAAAAAB8Y/PXkFGcMqiaESLCxaEmMsSE6kIL1q7zWyACLcBGAsYHQ/w1200-h630-p-k-no-nu/Screenshot_2019-09-18-19-30-10.png",Vale das artes
https://59f1cbe63db89.streamlock.net:1443/tvvaledasartes/_definst_/tvvaledasartes/chunklist_w427821205.m3u8

#EXTINF:-1 tvg-logo="http://www.canadatvmedia.com/wp-content/themes/canadatvmedia/images/ctv.jpg", C TV 
https://5d2c98775bafe.streamlock.net/8020/8020/chunklist_w50171203.m3u8

#EXTINF:-1 tvg-logo="http://photos1.blogger.com/blogger/4210/2628/1600/Figura122.jpg",TV Pombal
https://stmv1.samcast.com.br/tvpombal/tvpombal/chunklist_w1671185303.m3u8

#EXTINF:-1 tvg-logo="https://image.freepik.com/free-vector/retro-television-icon_1063-31.jpg",Vintage TV 
https://stmv1.srvstm.com/rogerio4248/rogerio4248/chunklist_w909976452.m3u8

#EXTINF:-1 tvg-logo="https://image.freepik.com/fotos-gratis/numero-29_2227-915.jpg",Canal 29
https://59f1cbe63db89.streamlock.net:1443/canal/_definst_/canal/chunklist_w542504344.m3u8


#EXTINF:-1 tvg-logo="https://2.bp.blogspot.com/-CfZuKXRoXtc/Ut5ISoI3wSI/AAAAAAAAVRg/I1wVT-AdJUY/s1600/Capital-TV.png" group-title="Washington", Tv Capital
http://wse01.logicahost.com.br:1935/tvcapital/_definst_/tvcapital/chunklist.m3u8

#EXTINF:-1 tvg-logo="https://upload.wikimedia.org/wikipedia/en/thumb/8/8f/Nova_logo_2011.jpg/150px-Nova_logo_2011.jpg" group-title="Washington", TV NOVA ONDA
https://5c483b9d1019c.streamlock.net/8078/8078/chunklist.m3u8


#EXTINF:-1 tvg-logo="https://logosave.com/images/large/16/Trianon-logo.png",TV Trianon
https://srv2.zcast.com.br/cleto2085/cleto2085/chunklist_w760721439.m3u8

#EXTINF:-1 tvg-logo="http://1.bp.blogspot.com/-zoZ81I7JjxA/Ui-51wlN_iI/AAAAAAAA0CQ/b33KpNlQWFU/s1600/00-tv_tupi_logo-600x338.jpg",TV Tupi
https://stmv1.srvif.com/redetupi/redetupi/chunklist_w1999240732.m3u8

#EXTINF:-1 tvg-logo="http://1.bp.blogspot.com/-jhXZbn4NKeM/Te_axMUfJHI/AAAAAAAAAAw/gzb9aZdL-Ao/s1600/logo_TV_LIBERDADE_qiideias_fim.jpg",TV Liberdade
http://01.paineldevideo.com/rtguarany/rtguarany/chunklist_w941692935.m3u8

#EXTINF:-1 tvg-logo="http://www.cxtv.com.br/img/Tvs/Logo/f24b1cd1064865c050503ea604e4526a.jpg",TV J.Sid,
https://livefocamundo.com:8081/8650/tracks-v1a1/mono.m3u8

#EXTINF:-1 tvg-logo="https://upload.wikimedia.org/wikipedia/pt/thumb/8/8a/Logotipo_da_TV_Imperial.JPG/200px-Logotipo_da_TV_Imperial.JPG",TV Imperial
https://5b7f3c45ab7c2.streamlock.net/8002/8002/chunklist_w119200362.m3u8

#EXTINF:-1 tvg-logo="https://upload.wikimedia.org/wikipedia/commons/4/4a/Latina_televisión_12_2019.jpg",Latina Sat
https://livefocamundo.com:8081/8470/tracks-v1a1/mono.m3u8

#EXTINF:-1 tvg-logo="http://panteao.com/wp-content/uploads/2015/12/apple-tv-logo-sharp.png",Genial TV 
https://studio.genialltv.com/live/Stream1_360p/chunklist_w1386099561.m3u8


#EXTINF:-1 tvg-logo="https://carioca.com/img/image_sudinoi/logo_carioca_sudinoi.png",carioca net
https://srv5.zcast.com.br/tvcarioca/tvcarioca/chunklist_w2007306206.m3u8

#EXTINF:-1 tvg-logo="https://i1.wp.com/bastidoresdatv.com.br/wp-content/uploads/2016/06/tv_brasil.jpg",TV Mais Brasil
https://srv5.zcast.com.br/tvmaisbrasil/tvmaisbrasil/chunklist_w137992942.m3u8

#EXTINF:-1 tvg-logo="https://upload.wikimedia.org/wikipedia/pt/3/3f/Logotipo_TV_Jornal.jpg",TV Jornal Nordeste
https://5cf4a2c2512a2.streamlock.net/jornaldonorteste/jornaldonorteste/chunklist_w728898588.m3u8

#EXTINF:-1 tvg-logo="http://superior-clean.co.uk/wp-content/uploads/2017/03/tv-icon.png",TV Osório 
https://5d82644094cc0.streamlock.net/tvosorionews/tvosorionews/chunklist_w1079896294.m3u8

#EXTINF:-1 tvg-logo="https://www.japi.com.br/wp-content/uploads/2019/11/logosite.png",TV Japi
https://srv2.zcast.com.br/tvjapi/tvjapi/chunklist_w1738551687.m3u8

#EXTINF:-1 tvg-logo="https://upload.wikimedia.org/wikipedia/en/thumb/3/38/HTV_logo_generic_1970-1992.svg/1280px-HTV_logo_generic_1970-1992.svg.png",H TV 
https://stmv3.samcast.com.br/hugo3090/hugo3090/chunklist_w51765378.m3u8


#EXTINF:-1 tvg-logo="https://is4-ssl.mzstatic.com/image/thumb/Purple60/v4/22/75/ef/2275efc3-21bb-d3a5-2315-982707b52e78/source/512x512bb.jpg" group-title="INTERNACIONAL",WOW TV HD El Salvador
http://cdn.elsalvadordigital.com:1935/wowtv/wowtv/playlist.m3u8


#EXTINF:-1 type="stream" tvg-id="A&E" tvg-logo="https://upload.wikimedia.org/wikipedia/commons/thumb/d/df/A%26E_Network_logo.svg/1000px-A%26E_Network_logo.svg.png" group-title="Filme e Serie",A&E
http://wse01.logicahost.com.br:1935/tvcapital/_definst_/tvcapital/chunklist_w1247683792.m3u8?EDSONSOUSALIST


#EXTINF:-1 type="stream" tvg-id="TV Brasil" tvg-logo="http://revenda.kudaplay.tv/tema/canais_logo/SD_TV_BRASIL.png" group-title="TV Aberta",TV Brasil
http://streaming.procergs.com.br:1935/tve/stve/playlist.m3u8

#EXTINF:-1 tvg-logo="https://listaiptv.gratis/logos/imagens/kuriakos2bkids.png" group-title="Infantil", TV Destak
http://wse01.logicahost.com.br:1935/pascoal/_definst_/pascoal/chunklist.m3u8

#EXTINF:-1 tvg-logo="https://listaiptv.gratis/logos/imagens/tv_evangelizar.png" group-title="Religioso",TV CANÇÃO NOVA HD
http://tvajuhls-lh.akamaihd.net:80/i/tvdesk_1@147040/index_1080_av-p.m3u8?EDSONSOUSALIST

#EXTINF:-1 type="stream" tvg-id="Terra Viva" tvg-logo="http://revenda.kudaplay.tv/tema/canais_logo/SD_TERRA_VIVA.png" group-title="TV Aberta",Terra Viva
http://evpp.mm.uol.com.br:1935/band_live/terraviva/playlist.m3u8?PEDROJUNIORTUTORIAIS

#EXTINF:-1 tvg-logo="https://upload.wikimedia.org/wikipedia/commons/thumb/f/f1/OnAir_Logo_Boomerang_HD_2015.png/640px-OnAir_Logo_Boomerang_HD_2015.png" group-title="Infantil",Boomerang
http://livestreamcdn.net:1935/ExtremaTV/ExtremaTV/playlist.m3u8

#EXTINF:-1 tvg-logo="https://lut.im/4dpYzeY5ga/LxqrxrjkXcIeOSn1.png" group-title="RELIGIOSO", TV APARECIDA  HD
http://69.46.0.170:1935/tvaparecida/tvaparecida.stream/live.m3u8?EDSONSOUSALIST


#EXTINF:-1 tvg-logo="http://i.imgur.com/ClaFSFI.png" group-title="FILMES/SERIES",TV CLASSIQUE
http://stmv2.euroti.com.br:1935/classique/classique/playlist.m3u8

#EXTINF:-1 tvg-logo="https://listaiptv.gratis/logos/imagens/retro-logo-bile2.png" group-title="Música",RETRO MUSIC
http://stream.mediawork.cz/retrotv//retrotvHQ1/chunklist_w119998573.m3u8

#EXTINF:-1 tvg-logo="http://listaiptvbrasil.com/logos/redemeionorte.png" group-title="Abertos", REDE PREMIUM 
https://slbps-sambatech.akamaized.net/live/3410%2C8879%2C12263efc906e7f003af8490bfe3c4941%3Bbase64np%3BeUxpdAC7dOnee8E%21/amlst%3AeUyRbR3wcejBO03M/chunklist.m3u8

#EXTINF:-1 tvg-logo="https://listaiptv.gratis/logos/imagens/retro-logo-bile2.png" group-title="Música", Retrô TV
http://stream.mediawork.cz/retrotv/retrotvHQ1/playlist.m3u8

#EXTINF:-1 tvg-logo="https://i.imgur.com/63s1k43.png" group-title="MUSICAS", PORTAL FOXMIX
http://149.56.17.92:1935/portalfoxmix/_definst_/portalfoxmix/playlist.m3u8


#EXTINF:-1 tvg-logo="https://listaiptv.gratis/logos/imagens/logo_top_latino_png_2017_-_500px.png" group-title="Música", Top TV
http://173.236.10.10:1935/toptv/toptv/chunklist_w233160029.m3u8?EDSONSOUSALIST

#EXTINF:-1 tvg-logo="https://cdn.mitvstatic.com/channels/record_m.jpg" group-title="RECORD",PT: TV RECORD SP
http://playplusspo-lh.akamaihd.net/i/pp_sp@350176/index_360_av-p.m3u8

#EXTINF:-1 type="stream" tvg-id="History" tvg-logo="https://upload.wikimedia.org/wikipedia/commons/thumb/f/f5/History_Logo.svg/985px-History_Logo.svg.png" group-title="Documentario",History
https://m.facebook.com/watch/live/?v=738746900380484&ref=watch_permalink&_rdr


#EXTINF:-1 tvg-logo="https://listaiptv.gratis/logos/imagens/izkxzj9.png" group-title="Religioso", Novo Tempo
http://stream.novotempo.com:1935/tv/smil:tvnovotempo.smil/playlist.m3u8


#EXTINF:-1 tvg-logo="https://listaiptv.gratis/logos/imagens/redbull.png" group-title="Esportes", Red Bull TV
https://rbmn-live.akamaized.net/hls/live/590964/BoRB-AT/master_3360.m3u8


#EXTINF:-1 tvg-logo="https://listaiptv.gratis/logos/imagens/200px-rede_vida_logo.png" group-title="Religioso",Rede Vida HD
https://cvd1.cds.ebtcvd.net/live-redevida/smil:redevida.smil/chunklist_b2128000.m3u8

#EXTINF:0 tvg-logo="https://pbs.twimg.com/profile_images/539378282978742273/-kOhKE4u_400x400.png" group-title="NOTICIAS", RADIO JORNAL TV
http://evpp.mm.uol.com.br/ne10/radiojornal-recife-avra-video-web.sdp/chunklist_w653587283.m3u8

#EXTINF:-1 tvg-logo="https://listaiptv.gratis/logos/imagens/1280px-bloomberg_television_logo.svg_.png" group-title="Jornalismo", Bloomberg TV
https://liveproduseast.akamaized.net/us/Channel-USTV-AWS-virginia-1/Source-USTV-1000-1_live.m3u8


#EXTINF:-1 tvg-id="Fox Channel" tvg-logo="http://s.eyplay.io/logos/canais/fox.png" group-title="Fox",Fox SD HITS
http://live.streams.ovh:1935/foxtv/foxtv/playlist.m3u8

#EXTINF:-1 tvg-logo="https://i.postimg.cc/0jxZtx5F/Polish-20191124-184241726.jpg" group-title="GOVERNO", catv
http://wowza4.catve.com.br:1935/mastertv/livestream/live.m3u8


#EXTINF:-1 tvg-logo="https://i.imgur.com/vR2sSns.jpg" group-title="Música", TV TOP Music Top
http://live-edge01.telecentro.net.ar/live/smil:musictop.smil/playlist.m3u8?IMDSFULL


#EXTINF:-1 type="stream" tvg-id="History" tvg-logo="https://upload.wikimedia.org/wikipedia/commons/thumb/f/f5/History_Logo.svg/985px-History_Logo.svg.png" group-title="Documentario",History LAB
http://wma10.fluidstream.net:1935/HistoryLab/livestream/chunklist_w657994700.m3u8

#EXTINF:-1 tvg-logo="https://i.imgur.com/yl6sJ46.jpg" group-title="Washington", Entretenimento TV
https://5c483b9d1019c.streamlock.net/8078/8078/chunklist.m3u8

#EXTINF:-1 tvg-logo="https://i.imgur.com/8IyVTYj.jpg"group-title="Washington", BRZ tv
https://oj7lng29dg82-hls-live.5centscdn.com/lives/f7b44cfafd5c52223d5498196c8a2e7b.sdp/index.m3u8


#EXTINF:-1 type="stream" tvg-id="National Geographic" tvg-logo="http://revenda.kudaplay.tv/tema/canais_logo/SD_NAT_GEO.png" group-title="Documentario", NAT GEO
http://tv02.logicahost.com.br:1935/tvdigitalbirigui/tvdigitalbirigui/live.m3u8


#EXTINF:-1 tvg-logo="https://i.imgur.com/yl6sJ46.jpg" group-title="Washington", TV Blumenau
https://cdn.jmvstream.com/w/LVW-8538/LVW8538_KBtZ9UMIZn/playlist.m3u8

#EXTINF:-1 tvg-logo="https://i.imgur.com/yl6sJ46.jpg" group-title="Washington", Rico'tv
https://video01.kshost.com.br/canalricos548/canalricos548/playlist.m3u8


#EXTINF:-1 tvg-logo="http://i.imgur.com/ClaFSFI.png" group-title="FILMES/SERIES",TELECINE TV Akati
https://livefocamundo.com:8081/8148/tracks-v1a1/mono.m3u8

#EXTINF:-1 tvg-logo="http://i.imgur.com/ClaFSFI.png" group-title="FILMES/SERIES",TELECINE 2
http://stmv2.euroti.com.br:1935/classique/classique/live.m3u8

#EXTINF:-1 tvg-logo="http://i.imgur.com/ClaFSFI.png" group-title="FILMES/SERIES",TELECINE 3
http://tv02.logicahost.com.br:1935/bonner/bonner/live.m3u8

#EXTINF:-1 tvg-logo="http://i.imgur.com/ClaFSFI.png" group-title="FILMES/SERIES",TELECINE
http://170.178.189.66:1935/live/Stream1/playlist.m3u8


#EXTINF:-1 type="stream" tvg-id="TV Brasil" tvg-logo="http://revenda.kudaplay.tv/tema/canais_logo/SD_TV_BRASIL.png" group-title="TV Aberta",TV Brasil
http://streaming.procergs.com.br:1935/tve/stve/playlist.m3u8

#EXTINF:0 tvg-logo="https://i.imgur.com/Agw8tsJ.png" group-title="CANAIS ABERTOS", TV EDUCATIVA
http://rbc.directradios.com:1935/rbc/smil:rbc.smil/playlist.m3u8

#EXTINF:-1 tvg-logo="https://i.ibb.co/CvvkxnR/tvcultura.png" group-title="Cultura", TV CULTURA  PA
http://177.74.1.38/funtelpa/tv_funtelpa/playlist.m3u8

#EXTINF:-1 tvg-logo="https://listaiptv.gratis/logos/imagens/catve.png" group-title="Cultura",Catve - CATVE HD
http://wowza4.catve.com.br:1935/live/livestream/playlist.m3u8?LISTASDAGLAU

#EXTINF:-1 type="stream" tvg-id="HBO" tvg-logo="http://revenda.kudaplay.tv/tema/canais_logo/SD_HBO.png" group-title="Filme e Serie",HBO 1
https://59f2354c05961.streamlock.net:1443/tvideonews/_definst_/tvideonews/chunklist.m3u8?IMDSFULL

#EXTINF:-1 tvg-logo="https://i.postimg.cc/0jxZtx5F/Polish-20191124-184241726.jpg" group-title="GOVERNO",TV Câmara
https://stream3.camara.gov.br/tv1/manifest.m3u8

#EXTINF:-1 tvg-logo="https://i.postimg.cc/0jxZtx5F/Polish-20191124-184241726.jpg" group-title="GOVERNO",TV Senado
http://www.tvmpf.mpf.mp.br:1935/event455/event455.stream_720p/playlist.m3u8
#EXTINF:-1 tvg-logo="https://i.postimg.cc/0jxZtx5F/Polish-20191124-184241726.jpg" group-title="GOVERNO",TV Justiça
http://www.tvmpf.mpf.mp.br:1935/event1900/event1900.stream_720p/playlist.m3u8
#EXTINF:-1 tvg-logo="https://i.postimg.cc/0jxZtx5F/Polish-20191124-184241726.jpg" group-title="GOVERNO",TV Escola
https://video-rvd-rj-gw-1.rnp.br/live/ocp(t(FgO7_EmATXI)r(TOqkzw)a(qHlmcw)p(d(lCo)k(ow8)m(C5-egTSp1-0Xr9xpSRQL6Q)n(a(CODpYw)s(H2c)'a(Tt4OSQ)s(_n4)))s(s(p4g)b(O-Sqy_e_0YeOeC8B48Ga1sjgoO2i82M2zB7qD38yiP0-ez02f0jUksn7wye4IgQ1OrKULuXwVKmoOvdIGw)'s(PSY)b(lsIlUWdKhX3NMey8pbgzSArpb0JQZ9AmMX-51AKS-Tcy1mDWugCsEQ29O9B6S91yYewyMBA195sJSwyeAsOnM33FdOaca5tmXCIByIqa)'s(NZ0)b(1tlzcFGL))m(0))/index.m3u8


#EXTINF:-1 tvg-logo="https://i.postimg.cc/7YwQ91fh/IMG-20180818-WA011-1-scale-2.png" group-title="ABERTO",tv camara
http://stream2.camara.gov.br:80/tv2/manifest.m3u8
-------------------------------------------------------
#EXTINF:-1 tvg-logo="https://i.postimg.cc/7YwQ91fh/IMG-20180818-WA011-1-scale-2.png" group-title="RELIGIOSO",rbc
http://rbc.directradios.com:1935/rbc/smil:rbc.smil/playlist.m3u8?leogondim          

#EXTINF:-1 tvg-logo="https://i.postimg.cc/7YwQ91fh/IMG-20180818-WA011-1-scale-2.png" group-title="RELIGIOSO", IURD TV
https://14398c.ha.azioncdn.net/primary/smil:tv_universal.smil/chunklist.m3u8

#EXTINF:-1 tvg-logo="https://i.postimg.cc/7YwQ91fh/IMG-20180818-WA011-1-scale-2.png" group-title="RELIGIOSO", IMPD TV
https://58a4464faef53.streamlock.net/impd/ngrp:impd_all/playlist.m3u8


-------------------------------------------------------
#EXTINF:-1 tvg-logo="https://i.postimg.cc/L61CJkFk/images.jpg" group-title="ABERTO",TVE
http://selpro1348.procergs.com.br:1935/tve/stve/playlist.m3u8
#EXTINF:-1 tvg-logo="https://i.postimg.cc/L61CJkFk/images.jpg" group-title="ABERTO",TVE BA
http://stream2.ba.gov.br/hls-live/livepkgr/_definst_/irdeb/pgm-1.m3u8

#EXTINF:-1 tvg-logo="https://i.postimg.cc/L61CJkFk/images.jpg" group-title="ABERTO", Rede Brasil
https://59f2354c05961.streamlock.net:1443/rbtv/_definst_/rbtv/playlist.m3u8
#EXTINF:-1 tvg-logo="https://i.postimg.cc/L61CJkFk/images.jpg" group-title="ABERTO", TV Camara
https://stream3.camara.gov.br/tv1/manifest.m3u8

-------------------------------------------------------
#EXTINF:-1 tvg-logo="https://i.postimg.cc/XNP4TM7n/Polish-20200617-000135235.jpg" group-title="TV LOCAL",tv parana
http://200.189.113.201/hls/tve.m3u8?leogondim
#EXTINF:-1 tvg-logo="https://i.postimg.cc/XNP4TM7n/Polish-20200617-000135235.jpg" group-title="TV LOCAL",FAAP
http://midia.faap.br:80/faaplatamlive-live/stream_720/livestream.m3u8    
#EXTINF:-1 tvg-logo="https://i.postimg.cc/XNP4TM7n/Polish-20200617-000135235.jpg" group-title="TV LOCAL",Tv Arapuan
https://5b7f3c45ab7c2.streamlock.net/tvarapuan/ngrp:tvarapuan/chunklist.m3u8
#EXTINF:-1 tvg-logo="https://i.postimg.cc/XNP4TM7n/Polish-20200617-000135235.jpg" group-title="TV LOCAL", RBATV
https://5cf4a2c2512a2.streamlock.net/rbatv/rbatv/chunklist.m3u8

#EXTINF:-1 tvg-logo="https://i.postimg.cc/XNP4TM7n/Polish-20200617-000135235.jpg" group-title="TV LOCAL",TV Oeste MA
https://5d2c98775bafe.streamlock.net/8104/8104/playlist.m3u8
#EXTINF:-1 tvg-logo="https://i.postimg.cc/XNP4TM7n/Polish-20200617-000135235.jpg" group-title="TV LOCAL",TV Liberdade
http://01.paineldevideo.com/rtguarany/rtguarany/playlist.m3u8
#EXTINF:-1 tvg-logo="https://i.postimg.cc/XNP4TM7n/Polish-20200617-000135235.jpg" group-title="TV LOCAL", CBTV
https://oj7lng29dg82-hls-live.5centscdn.com/lives/f7b44cfafd5c52223d5498196c8a2e7b.sdp/index.m3u8

#EXTINF:-1 tvg-logo="https://i.postimg.cc/XNP4TM7n/Polish-20200617-000135235.jpg" group-title="TV LOCAL",TV Paraná Turismo
http://200.189.113.201/hls/tve.m3u8


#EXTINF:-1 tvg-logo="https://i.postimg.cc/XNP4TM7n/Polish-20200617-000135235.jpg" group-title="TV LOCAL",TV Santa Cecilia
http://flash1.crossdigital.com.br/2063/2063/chunklist.m3u8

#EXTINF:-1 tvg-logo="https://i.postimg.cc/XNP4TM7n/Polish-20200617-000135235.jpg" group-title="TV LOCAL",TV Digital Birigui
http://wse01.logicahost.com.br:1935/tvdigitalbirigui/_definst_/tvdigitalbirigui/chunklist.m3u8


#EXTINF:-1 tvg-logo="https://i.postimg.cc/XNP4TM7n/Polish-20200617-000135235.jpg" group-title="TV LOCAL",TV Max
https://59f1cbe63db89.streamlock.net:1443/tvmax/_definst_/tvmax/playlist.m3u8

#EXTINF:-1 tvg-logo="https://i.postimg.cc/XNP4TM7n/Polish-20200617-000135235.jpg" group-title="TV LOCAL",TV Diario do Sertao
http://painelvj.com.br:1935/pdsertaotv/pdsertaotv.sdp/playlist.m3u8

#EXTINF:-1 tvg-logo="https://i.postimg.cc/XNP4TM7n/Polish-20200617-000135235.jpg" group-title="TV LOCAL", Rede Metropole
http://177.130.81.30/hls/metropole.m3u8

#EXTINF:-1 tvg-logo="https://i.postimg.cc/XNP4TM7n/Polish-20200617-000135235.jpg" group-title="TV LOCAL",Zee Mundo
https://f8e7y4c6.ssl.hwcdn.net/mundohd/tracks-v1a1/index-1588722015-now.m3u8


#EXTINF:-1 tvg-logo="https://i.postimg.cc/0jxZtx5F/Polish-20191124-184241726.jpg" group-title="REPORTAGEM NEWS",CNN Brasil
https://streaming.cnnbrasil.com.br/cnndigital_main.m3u8
#EXTINF:-1 tvg-logo="https://i.postimg.cc/0jxZtx5F/Polish-20191124-184241726.jpg" group-title="REPORTAGEM NEWS", Record News SP
https://playplusnews-lh.akamaihd.net/i/pp_nws@377849/master.m3u8

#EXTINF:-1 tvg-logo="https://i.postimg.cc/0jxZtx5F/Polish-20191124-184241726.jpg" group-title="REPORTAGEM NEWS",ABC News
https://abclive1-lh.akamaihd.net/i/abc_live01@423395/index_2500_av-p.m3u8
#EXTINF:-1 tvg-logo="https://i.postimg.cc/0jxZtx5F/Polish-20191124-184241726.jpg" group-title="REPORTAGEM NEWS",CNN News 18


#EXTINF:-1 tvg-logo="http://i.imgur.com/wTPxeFu.jpg" group-title="INFORMAÇÕES", Documentários Além dos limites humanos hipoxia
http://archive.org/download/AlemDosLimitesHumanosDubladoDocumentario/02%20-%20Frio%5bmvm%5d.mp4

#EXTINF:-1 tvg-logo="http://i.imgur.com/wTPxeFu.jpg" group-title="INFORMAÇÕES", Documentários Além dos limites humanos frio
http://archive.org/download/AlemDosLimitesHumanosDubladoDocumentario/01%20-%20Hipoxia%5bmvm%5d.mp4

#EXTINF:-1 tvg-logo="http://i.imgur.com/wTPxeFu.jpg" group-title="INFORMAÇÕES", Documentários Além dos limites humanos tempo
http://archive.org/download/AlemDosLimitesHumanosDubladoDocumentario/03%20-%20Tempo%5bmvm%5d.mp4

#EXTINF:-1 tvg-logo="http://i.imgur.com/wTPxeFu.jpg" group-title="INFORMAÇÕES", Documentários 
Stephen Hawking e as células tronco
http://archive.org/download/Stephen.Hawking.e.as.Celulas-Tronco/C%c3%a9lulas-tronco%20com%20Stephen%20Hawking-PSG.mp4.mp4

#EXTINF:-1 tvg-logo="http://i.imgur.com/wTPxeFu.jpg" group-title="INFORMAÇÕES", Documentários 
Stephen Hawking e o sentido da vida
http://archive.org/download/OSentidoDaVidaPorStephenHawking/O%20Sentido%20da%20Vida%20por%20Stephen%20Hawking.mp4

#EXTINF:-1 tvg-logo="http://i.imgur.com/wTPxeFu.jpg" group-title="INFORMAÇÕES", Documentários Telescópio o universo revela
http://archive.org/download/Telescopio-2016/TELESC%c3%93PIO%20-%20O%20UNIVERSO%20REVELADO%20480p%20HDTV-Master.mp4
