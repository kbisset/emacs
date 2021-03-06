(setq nnmail-split-methods
      '(
        ("junk.pmx1" "^Subject:.*PMX\:\#.$")
        ("junk.pmx2" "^Subject:.*PMX\:\#\#.$")
        ("junk.pmx3" "^Subject:.*PMX\:\#\#\#.$")
        ("junk.pmx4+" "^Subject:.*PMX\:\#\#\#\#*.$")
        ("patterns.ipc" "^To:.*ipc-patterns.*")
        ("patterns.ipc" "^[Cc][Cc]:.*ipc-patterns.*")
         ("patterns.gof" "^X-Mailing-List:.*gang-of-4-patterns.*")
         ("patterns.tool" "^To:.*patterns-tool.*")
         ("patterns.tool" "^[Cc][Cc]:.*patterns-tool.*")
         ("patterns.d" "^To:.*patterns-discussion.*")
         ("patterns.d" "^[Cc][Cc]:.*patterns-discussion.*")
         ("patterns" "^To:.*patterns.*")
         ("patterns" "^[Cc][Cc]:.*patterns.*")
         ("news.slashdot" "From: slashdot@slashdot.org")
         ("lanl.listadmin" "From: listmanager@maillist.lanl.gov")
         ("software.tex.psfrag" "^To:.*psfrag@rascals.Stanford.EDU.*")
         ("software.tex.psfrag" "^[Cc][Cc]:.*psfrag@rascals.Stanford.EDU.*")
;;          ("software.tex.ctan" "^From:.*listserv@urz.uni-heidelberg.de.*")
;;          ("software.tex.ctan" "^To:.*CTAN-ANN.*")
         ("software.tex.ctan" "^Sender: owner-ctan-ann@.*dante.de")
         ("software.tex.ctan" "^To: ctan-ann@dante.de")
         ("software.tex.tetex.pretest" "^Sender: owner-tetex-pretest@.*")
         ("software.tex.tetex" "^Sender: owner-tetex@.*")
         ("software.tex.oztex" "^Sender: owner-oztex-info@.*")
         ("software.tex.latex3" "^Sender: Mailing list for the LaTeX3 project")
         ("software.gnu.make" "^Resent-Sender: .*-make-request@gnu.org")
         ("software.emacs.cc-mode" "^From: mailman-owner@python.org")
         ("software.emacs.w3" "^Resent-Sender: w3-beta-request@gnu.org")
         ("software.emacs.ecb" "^List-Id: Mailing list for ECB help")
         ("software.xlock" "^Return-Path: <owner-xlock@tux.org>")
         ("software.xlock" "^Sender: .*xlock.*@tux.org")
         ("software.redhat.utb" "From: under-the-brim.*@redhat.com")
         ("software.redhat.utb" "X-Mailing-List:.*under-the-brim@redhat.com")
         ("software.redhat" "From:.*rhn-admin@rhn.redhat.com")
         ("software.glomosim" "^To:.*GLOMOSIM-USERS-L@listserv.ucla.edu")
         ("software.cppunit" "^List-Id: <cppunit-devel.lists.sourceforge.net>")
         ("software.cppunit" "^List-Id: <cpptool-develop.lists.sourceforge.net>")
         ("software.boost.install" "^List-Id: Boost installation list")
         ("software.boost" "^List-Id: Boost announce-only mailing list")
         ("accu" "^From: majordomo@accu.org")
         ("accu.announce" "^Sender:.*announce@accu.org")
         ("accu.books" "^Sender:.*books@accu.org")
         ("accu.contacts" "^Sender:.*contacts@accu.org")
         ("accu.contacts" "^From: owner-accu-contacts@accu.org")
         ("accu.d-books" "^Sender:.*d-books@accu.org")
         ("accu.general" "^Sender:.*general@accu.org")
         ("accu.hosts" "^Sender:.*hosts@accu.org")
         ("accu.prog-questions" "^Sender:.*prog-questions@accu.org")
         ("accu-mentored-developers" "^To:.*accu-mentored-developers@accu.org")
         ("list.politech" "^Sender: owner-politech@vorlon.mit.edu")
         ("list.sigplan" "^Sender:.*SIGPLAN-ANNOUNCE@ACM.ORG")
         ("list.sigsim" "^Sender:.*SIGSIM@ACM.ORG")
         ("list.risks" "^Sender: .*@csl.sri.com")
         ("list.mini-air" "^Sender: mini-air@chem.harvard.edu")
         ("list.spie" "^From:.*@spie.org")
         ("list.spie" "^Mailing-List: contact optics-newsalert-owner")
         ("list.technews" "^Sender: ACM TechNews Early Alert Service")
         ("list.beowulf" "^From: beowulf-request@beowulf.org")
         ("list.beowolf" "^List-Id:.*beowulf.beowulf.org")
         ("list.lasc" "To:.*lasc@lanl.gov")
         ("list.tdd" "^Mailing-List:.*testdrivendevelopment@yahoogroups.com")
         ("list.refactoring" "^Mailing-List:.*refactoring@yahoogroups.com")
         ("list.sony" "^Mailing-List:.*sonyhs10@yahoogroups.com")
         ("list.sony.wega" "^Mailing-List:.*sony-wega@yahoogroups.com")
         ("lanl.ldrd-er" "^To: ldrd-csse@lanl.gov")
         ("vg" "^From: rkelsey@host5.webserver1010.com")
         ("vg" "^From:.*vgadmin@vinylgalore.com")
         ("vg" "^To:.*vgadmin@vinylgalore.com")
         ("junk.junk" "^Subject: cc:Mail Link to SMTP Undeliverable Message")
         ("junk.junk" "^From:.*MAILER-DAEMON@")
         ("junk.junk" "^To:.*keith@lanl.gov")
         ("junk.junk" "^From:.*specials.iomega.com")
         ("junk.junk" "From:.*@spotshop.com")
         ("junk.junk" "From:.*From:.*News@directv")
         ("junk.junk" "From:.*argotech.com")
         ("junk.junk" "From:.*smallcapnetwork.net")
         ("junk.junk" "From:.*@tigr.org")
         ("junk.junk" "^From:.*optics.org")
         ("junk.junk" "^From:.*nfobahn@attglobal.net")
         ("junk.junk" "^List-Owner:.*owner-smallcapdigest@lyris.smallcapnetwork.net")
         ("yahoo" "From: .*yahoo-inc.com")
         ("software.cvs" "^Return-Path:.*info-cvs-request@.*")
         ("software.cvs" "^X-Mailing-List:.*info-cvs@.*")
         ("software.xml4c" "^Mailing-List:.*xerces-dev-help@xml.apache.org")
         ("software.xml4c.general" "^Mailing-List:.*general-help@xml.apache.org")
         ("software.xml4c.cvs" "^Mailing-List:.*xerces-cvs-help@xml.apache.org")
         ("software.xml4c.announce" "^Mailing-List:.*announcements-help@xml.apache.org")
         ("software.xml4c.misc" "^Mailing-List:.*help@xml.apache.org")
         ("software.xml.ibm" "^From:.*cntr4xml@US.IBM.COM")
         ("software.xml.ibm" "^To:.*ALPHAFLASH@mail.software.ibm.com")
         ("software.linux.wizards" "Sender:.*Linux Wizards.*")
         ("software.linux.smp" "Sender: owner-linux-smp")
         ("software.linux.kernel" "Sender: owner-linux-kernel")
         ("software.linux.kernel" "To: linux-kernel-outgoing@vger.rutgers.edu")
         ("software.linux.admin" "Sender: owner-linux-admin")
         ("software.linux.gcc" "Sender: owner-linux-gcc")
         ("software.linux.config" "Sender: owner-linux-config")
         ("software.linux.apps" "Sender: owner-linux-apps")
         ("software.linux.x11" "Sender: owner-linux-x11")
         ("software.linux.ppp" "Sender: owner-linux-ppp")
         ("software.linux.mklinux.setup" "List-Id:.*mklinux-setup")
         ("software.linux.mklinux.announce" "List-Id:.*mklinux-announce")
         ("software.linux.mklinux.hardware" "List-Id:.*mklinux-hardware")
         ("software.linux.mklinux.users" "List-Id:.*mklinux-users")
         ("software.linux.lanl" "Sender:.*owner-linux@maillist.lanl.gov")
         ("software.linux.alert" "X-Loop: linux-alert@redhat.com")
         ("software.linux.mandrake.announce.security" "^Sender: security-announce-owner@linux-mandrake.com")
         ("software.linux.mandrake.announce" "^List-Owner:/* announce-request@linux-mandrake.com")
         ("software.pure" "From:.*PureAtria.COM.*")
         ("software.dxpc" "Sender:.*owner-dxpc@.*utexas.edu.*")
         ("software.lam" "From:.*lam-chops.*")
         ("software.lam" "To:.*lam-chops.*")
         ("software.lam" "[CC][cc]:.*lam-chops.*")
         ("software.lam" "To:.*lam@.*")
         ("software.fvwm" "Sender: owner-fvwm.*@hpc.uh.edu")
         ("software.fvwm" "From: Majordomo@hpc.uh.edu")
         ("software.gphoto" "^From:.*gphoto.org")
         ("software.gphoto" "^From:.*gphoto.net")
         ("software.gphoto" "^Sender:.*gphoto.org")
         ("software.gphoto" "^Sender:.*gphoto.net")
         ("software.gphoto" "^From: mailman-owner@lists.styx.net")
         ("software.gphoto" "^List-Subscribe:.*gphoto.net")
;;         ("software.gnumeric" "^List-Id:.*gnumeric-list.gnome.org")
         ("software.gnumeric" "^Sender: gnumeric-list-admin@gnome.org")
         ("software.linux.rio" "^List-Id:.*rio500-devel.lists.sourceforge.net")
         ("software.linux.rio" "^List-Id:.*rio500-users.lists.sourceforge.net")
         ("software.mac.security.announce" "^List-Id:.*security-announce.lists.apple.com")
         ("software.mac.x11.users" "^List-Id:.*x11-users.lists.apple.com")
         ("software.mac.unix.porting" "^List-Id:.*unix-porting.lists.apple.com")
         ("software.mac.darwinos.users" "^List-Id:.*darwinos-users.lists.apple.com")
         ("software.mac.fink.announce" "^List-Id:.*fink-announce.lists.sourceforge.net")
         ("lanl.tsa5.morale" "^To: d5morale@lanl.gov")
         ("lanl.tsa5.cluster" "^To:.*d5-cluster")
         ("lanl.tsa5.cluster" "^Cc:.*d5-cluster")
         ("jemez.com" "To:.*kbisset@jemez.com")
         ("jemez.com" "To:.*KBISSET@jemez.com")
         ("netflix" "From:.*@netflix.com")
         ("trail.com" "^Delivered-To:.*waldo@cyberhighway.net")
         ("Rob" "^From:.*rob@.*lanl.gov.*")
         ("Rob" "^From:.*rkelsey@palm.net")
         ("Rob" "^From:.*rkelsey@.*cs.nmsu.edu.*")
         ("Wendy" "^From:.*wcaswell@.*")
         ("Wendy" "^From:.*wendy@trail.com.*")
         ("Wendy" "^From:.*wcb@lanl.gov")
         ("Wendy" "^From:From:.*wendy@jemez.com")
         ("Pat" "^From:.*teller@cs.nmsu.edu.*")
         ("Pat" "^From:.*pteller@cs.utep.edu.*")
         ("Sumedh" "^From:.*Sumedh S Pathak")
         ("Sumedh" "^From:.*spathak@lanl.gov")
         ("conference.pads99" "^From: pads99@atlas.ex.ac.uk")
         ("lanl.students" "^.*mkolakow@hotmail.com")
         ("lanl.intelligent-actors" "^To:.*intelligent-actors@lanl.gov.*")
         ("lanl.jwars" "^To:.*jwars-developers@lanl.gov")
         ("lanl.chad" "^From:.*chadly@nmt.edu")
;         ("lanl.jwars" "^From:.*stevem@.*lanl.gov.*")
;         ("lanl.jwars" "^From:.*srich@.*lanl.gov.*")
;         ("lanl.jwars" "^From:.*tnr@.*lanl.gov.*")
;         ("lanl.jwars" "^From:.*rem@.*lanl.gov.*")
;         ("lanl.jwars" "^From:.*dhines@.*lanl.gov.*")
;         ("lanl.jwars" "^From:.*booker@.*lanl.gov.*")
;         ("lanl.jwars" "^From:.*cxb@.*lanl.gov.*")
;         ("lanl.jwars" "^From:.*upton@.*lanl.gov.*")
;         ("lanl.jwars" "^From:.*boggs.*")
;         ("lanl.jwars" "^From:.*seanrich@rt66.com.*")
;         ("lanl.jwars" "^From:.*mkosciel@.*lanl.gov.*")
         ("lanl.sourceforge" "^Sender: owner-sourceforge-users@maillist.lanl.gov")
         ("lanl.mug" "^Sender: owner-lanlmug@maillist.lanl.gov")
         ("lanl.sourceforge" "^Sender: <sf-httpd@sourceforge-web.lanl.gov>")
         ("lanl.building.sm43" "^From:.*bmsm43@lanl.gov")
         ("lanl.building.sm43" "^From:.*gladson_wiley_l_jr@lanl.gov")
         ("lanl.building.sm43" "^From:.*fogel@lanl.gov")
         ("lanl.building.sm43" "^From:.*tjtj@lanl.gov")
         ("lanl.building.sm43" "^Subject: BUILDING NOTIFICATION")
         ("lanl.distributions" "^From:.*distributions@lanl.gov")
         ("lanl.pa" "^From:.*paonotices@lanl.gov")
         ("lanl.deutsch" "^Sender: owner-deutschstunde@maillist.lanl.gov")
         ("lanl.esd" "^From: esdmaster@lanl.gov")
         ("list.barcode" "^Sender:.*owner-barcode@quantumlinux.com")
         ("list.barcode" "^Reply-To: barcode@quantumlinux.com")
         ("lanl.delibes" "^Sender: owner-delibes@maillist.lanl.gov")
         ("lanl.transims.metro" "^From: TRANSIMS-LANL <lanl@localhost.localdomain>")
         ("lanl.transims.metro" "^From:.*metro.dst.or.us")
         ("lanl.nisac.nightly-build" "^Subject:.*NB:")
         ("lanl.nisac.uis-software" "^To: owner-uis-software@maillist.lanl.gov")
         ("lanl.nisac.uis-software" "^To: uis-software-approval@maillist.lanl.gov")
         ("lanl.transims.cm.admin" "^To: uis-cvs-[a-z]*-approval@maillist.lanl.gov")
         ("lanl.nisac" "From:.*gam@lanl.gov")
         ("lanl.nisac" "From:.*prr@lanl.gov")
         ("lanl.nisac" "From:.*bosetti@lanl.gov")
         ("lanl.nisac" "From:.*nwang@cs.umd.edu")
         ("lanl.nisac" "From:.*gam@lanl.gov")
         ("lanl.nisac" "^From:.*jriese@lanl.gov")
         ("lanl.nisac" "^From:.*jcohn@lanl.gov")
         ("lanl.nisac" "^From:.*eidenben@lanl.gov")
         ("lanl.nisac" "^From:.*smm@lanl.gov")
         ("lanl.nisac" "^From:.*henning@lanl.gov")
         ("lanl.nisac" "^From:.*kdb@lanl.gov")
         ("lanl.nisac" "^From:.*istrate")
         ("lanl.nisac" "^From:.*anil.*lanl.gov")
         ("lanl.nisac" "^From:.*madhav@.*lanl.gov")
         ("lanl.nisac" "^Sender: owner-mobicom@maillist.lanl.gov")
         ("lanl.nisac" "^From:.*Monique Morin")
         ("lanl.nisac" "^From:.*Achla Marathe")
         ("lanl.nisac" "^From:.*kroc@lanl.gov")
         ("lanl.nisac" "^From:.*engelhart@lanl.gov")
         ("lanl.nisac" "^From:.*zust@lanl.gov")
         ("lanl.nisac" "^From:.*karla@lanl.gov")
         ("lanl.transims.sa" "^From:.*shull@lanl.gov")
         ("lanl.transims.cm" "^Subject:.*Code Manager notification")
         ("lanl.transims.cm" "^Subject:.*Mobicom CVS Notification")
         ("lanl.transims.cm" "^Subject:.*CVS commit message")
         ("lanl.transims.cm" "^Subject:.*CVS commit message")
         ("lanl.transims.cm" "^From:.*cvs@evans.tsasa.lanl.gov")
         ("lanl.nisac" "^From:.*llsmith@lanl.gov")
         ("lanl.nisac" "^From:.*kubicek@.*lanl.gov")
         ("lanl.nisac" "^From:.*stretz@.*lanl.gov")
         ("lanl.nisac" "^From:.*goran@lanl.gov")
         ("lanl.nisac" "^From:.*bwb@lanl.gov")
         ("lanl.nisac" "^From:.*dick@.*lanl.gov")
         ("lanl.nisac" "^From:.*r@lanl.gov")
         ("lanl.nisac" "^From: Kathryn Berkbigler")
         ("lanl.transims" "^Sender: owner-transims-sw@maillist.lanl.gov")
         ("lanl.ccs5" "^From:.*tsasa.lanl.gov")
         ("lanl.nisac" "^From:.*eubank@*lanl.gov")
         ("lanl.nisac" "^From:.*dzzr@lanl.gov")
         ("lanl.nisac" "^From:.*sxs@lanl.gov")
         ("lanl.nisac" "^From:.*pistone@lanl.gov")
         ("lanl.nisac" "^From:.*jmh@lanl.gov")
         ("lanl.nisac" "^From:.*Marcus Rickert")
         ("lanl.nisac" "^From:.*barrett")
         ("lanl.nisac" "^From:.*kpb@c3serve.c3.lanl.gov")
         ("lanl.nisac" "^From:.*jpsmith@lanl.gov")
         ("lanl.nisac" "^From:.*Timothy.*Cleland")
         ("lanl.ccs5" "^From:.*tel@lanl.gov")
         ("lanl.ccs5" "^From:.*haway@lanl.gov")
         ("lanl.transims" "^To:.*transims.*")
         ("lanl.nisac" "^From:.*kriste@lanl.gov")
         ("lanl.nisac" "^From: Madhav Marathe")
         ("lanl.alert" "^Subject: Explorer Alert:")
         ("lanl.alert" "From: policy@lanl.gov")
         ("lanl.acl" "^Sender: .*acl.lanl.gov")
         ("lanl.acl" "^To: keith@acl.lanl.gov")
         ("lanl.d4" "^From:.*esther@lanl.gov")
         ("lanl.d4" "^From:.*emvan@lanl.gov")
         ("lanl.d4" "^From:.*sjf@lanl.gov")
         ("lanl.ccs5.staff" "^To:.*ccs5staff@lanl.gov")
         ("lanl.ccs5" "^To:.*ccs5_pueblo@lanl.gov")
         ("lanl.ccs5" "^From:.*anson@lanl.gov")
         ("lanl.ddo" "^From:.*jrogers@lanl.gov")
         ("lanl.ddo" "^From:.*devaurs_micheline@lanl.gov")
         ("lanl.ddo" "^From:.*dat@lanl.gov")
         ("lanl.sci-alerts" "^From:.*sci-alerts@lanl.gov")
         ("lanl.sci-alerts" "^To:.*scialerts@mantaray.lanl.gov.*")
         ("lanl.students" "^To:.*students@lanl.gov.*")
         ("lanl.students" "^.*To:.*sa-all*")
         ("lanl.students" "^[Cc][Cc]:.*sa-all*")
         ("lanl.wisdom" "^X-From-Line: owner-wisdom")
         ("lanl.hi" "^From: Phil Jacobson")
         ("lanl.hi" "^From:.*Brouillette")
         ("junk.junk" "^Subject:.*Today's Headlines")
         ("junk.junk" "^From: adm <adm@delibes.lanl.gov>")
         ("junk.junk" "^From:.*spam@lanl.gov")
         ("lanl.ccs" "^To: ccs-all@lanl.gov")
         ("lanl.other" "^From:.*.lanl.gov")
         ("junk.junk" "^Subject:.*Output from \"cron\"")
         ("junk.junk" "^From:.*Software Development Magazine")
         ("junk.junk" "^From:.*LADAIntnet@aol.com")
         ("junk.junk" "^From:.*sdr@dsizone.net")
         ("junk.junk" "^Subject:.*mailing list memberships reminder")
         ("junk.junk" "^From:.*sdmail@dsi-sdmag.net")
         ("from.nmsu" "^X-Loop: kbisset@cs.nmsu.edu")
         ("from.nmsu" "^From:.*kbisset@cs.nmsu.edu")
         ("cs" "^To:.*cs.*@cs.nmsu.edu")
         ("from.nmsu" "^From:.*@.*.nmsu.edu.*")
         ("trail" "^From.*@trail.com")
         ("junk.junk" "^Content-Type:.*multipart/alternative")
         ("other" "")))


