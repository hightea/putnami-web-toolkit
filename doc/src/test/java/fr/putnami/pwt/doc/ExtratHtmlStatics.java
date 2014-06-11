package fr.putnami.pwt.doc;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.MalformedURLException;

import com.gargoylesoftware.htmlunit.BrowserVersion;
import com.gargoylesoftware.htmlunit.FailingHttpStatusCodeException;
import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.DomNode;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.google.common.io.Files;

import fr.putnami.pwt.core.mvp.client.MvpPlace;
import fr.putnami.pwt.doc.client.page.binding.DataBindingPlace;
import fr.putnami.pwt.doc.client.page.bootstrap.BootstrapPlace;
import fr.putnami.pwt.doc.client.page.components.ComponentsPlace;
import fr.putnami.pwt.doc.client.page.errors.ErrorsPlace;
import fr.putnami.pwt.doc.client.page.form.FormsPlace;
import fr.putnami.pwt.doc.client.page.i18n.InternationalizationPlace;
import fr.putnami.pwt.doc.client.page.layout.LayoutsPlace;
import fr.putnami.pwt.doc.client.page.more.MorePlace;
import fr.putnami.pwt.doc.client.page.navigation.NavigationPlace;
import fr.putnami.pwt.doc.client.page.plugins.CodeEditorPlace;
import fr.putnami.pwt.doc.client.page.sample.addressbook.AddressBookPlace;
import fr.putnami.pwt.doc.client.page.sample.all.SamplesPlace;
import fr.putnami.pwt.doc.client.page.sample.table.ContactsTablePlace;
import fr.putnami.pwt.doc.client.page.server.ServerCallsPlace;
import fr.putnami.pwt.doc.client.page.soon.CommingSoonPlace;
import fr.putnami.pwt.doc.client.page.starting.GettingStartedPlace;
import fr.putnami.pwt.doc.client.page.table.TablesPlace;
import fr.putnami.pwt.doc.client.page.welcome.WelcomePlace;

public class ExtratHtmlStatics {

	private static final String targetHost = "http://gwt.putnami.org/";
	private static final String host = "http://localhost:8080/pwt-doc/";
	private static final String outFolder = "build/htmlStatics/";
	private static final File siteMap = new File(outFolder + "sitemap.txt");
	private static WebClient webClient;
	
	public static void main(String[] args) {
		 webClient = new WebClient(BrowserVersion.CHROME);
		siteMap.delete();
		new File(outFolder).mkdirs();
		try {
			extractPage( WelcomePlace.class);
			extractPage(GettingStartedPlace.class);
			extractPage(BootstrapPlace.class);
			extractPage(LayoutsPlace.class);
			extractPage(ComponentsPlace.class);
			extractPage(FormsPlace.class);
			extractPage(TablesPlace.class);
			extractPage(MorePlace.class);
			extractPage(DataBindingPlace.class);
			extractPage(InternationalizationPlace.class);
			extractPage(NavigationPlace.class);
			extractPage(ServerCallsPlace.class);
			extractPage(ErrorsPlace.class);
			extractPage(CodeEditorPlace.class);
			extractPage(SamplesPlace.class);
			extractPage(ContactsTablePlace.class);
			extractPage(AddressBookPlace.class);
			extractPage(CommingSoonPlace.class);
			
			Files.copy(new File(outFolder + "Welcome.html"), new File(outFolder + "index.html"));
		}
		catch (FailingHttpStatusCodeException | IOException e) {
			throw new RuntimeException(e);
		}

	}

	private static void extractPage(Class<? extends MvpPlace> place)
			throws FailingHttpStatusCodeException, MalformedURLException, IOException {

		String tokenName = null;
		HtmlPage page = null;
		String url = host;
		if (place != null) {
			tokenName = place.getSimpleName().replace("Place", "");
			url += "#!" + tokenName;
		}
		page = webClient.getPage(url);
//		for (DomNode node : page.getHead().getChildren()) {
//			if ("script".equals(node.getNodeName())) {
//				node.remove();
//			}
//		}
		String pageData = page.asXml();
		pageData = pageData.replaceAll(host, "");

		String outFile = outFolder ;
		if (tokenName != null) {
			outFile += tokenName;
		}
		outFile += ".html";
		PrintWriter out = new PrintWriter(outFile);
		out.print(pageData);
		out.close();

		PrintWriter siteMapWriter = new PrintWriter(new BufferedWriter(new FileWriter(siteMap, true)));
		siteMapWriter.println(targetHost + tokenName + ".html");
		siteMapWriter.close();

		webClient.closeAllWindows();
	}
}
