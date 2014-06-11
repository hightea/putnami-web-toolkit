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

	private static final String host = "http://localhost:8080/pwt-doc/";
	private static final String outFolder = "build/htmlStatics/";
	private static final File siteMap = new File(outFolder + "sitemap.txt");

	public static void main(String[] args) {
		final WebClient webClient = new WebClient(BrowserVersion.CHROME);

		new File(outFolder).mkdirs();
		try {
			extractPage(webClient, null);
			extractPage(webClient, WelcomePlace.class);
			extractPage(webClient, GettingStartedPlace.class);
			extractPage(webClient, BootstrapPlace.class);
			extractPage(webClient, LayoutsPlace.class);
			extractPage(webClient, ComponentsPlace.class);
			extractPage(webClient, FormsPlace.class);
			extractPage(webClient, TablesPlace.class);
			extractPage(webClient, MorePlace.class);
			extractPage(webClient, DataBindingPlace.class);
			extractPage(webClient, InternationalizationPlace.class);
			extractPage(webClient, NavigationPlace.class);
			extractPage(webClient, ServerCallsPlace.class);
			extractPage(webClient, ErrorsPlace.class);
			extractPage(webClient, CodeEditorPlace.class);
			extractPage(webClient, SamplesPlace.class);
			extractPage(webClient, ContactsTablePlace.class);
			extractPage(webClient, AddressBookPlace.class);
			extractPage(webClient, CommingSoonPlace.class);
		}
		catch (FailingHttpStatusCodeException | IOException e) {
			throw new RuntimeException(e);
		}

	}

	private static void extractPage(WebClient webClient, Class<? extends MvpPlace> place)
			throws FailingHttpStatusCodeException, MalformedURLException, IOException {
		String tokenName = null;
		HtmlPage page = null;
		String url = host;
		if (place != null) {
			tokenName = place.getSimpleName().replace("Place", "");
			url += "#!" + tokenName;
		}
		page = webClient.getPage(url);

		for (DomNode node : page.getHead().getChildren()) {
			if ("script".equals(node.getNodeName())) {
				node.remove();
			}
		}

		String pageData = page.asXml();
		pageData = pageData.replaceAll(host, "");

		String outFile = outFolder + "?_escaped_fragment_";
		if (tokenName != null) {
			outFile += "=" + tokenName;
		}
		PrintWriter out = new PrintWriter(outFile);
		out.print(pageData);
		out.close();

		PrintWriter siteMapWriter = new PrintWriter(new BufferedWriter(new FileWriter(siteMap, true)));
		siteMapWriter.println(url);
		siteMapWriter.close();

	}
}
