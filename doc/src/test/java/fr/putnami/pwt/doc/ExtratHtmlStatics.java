/**
 * This file is part of pwt-doc.
 *
 * pwt-doc is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt-doc is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt-doc.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.doc;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.MalformedURLException;
import java.util.List;

import org.w3c.dom.NamedNodeMap;

import com.gargoylesoftware.htmlunit.BrowserVersion;
import com.gargoylesoftware.htmlunit.FailingHttpStatusCodeException;
import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.DomAttr;
import com.gargoylesoftware.htmlunit.html.DomElement;
import com.gargoylesoftware.htmlunit.html.DomNode;
import com.gargoylesoftware.htmlunit.html.HtmlDivision;
import com.gargoylesoftware.htmlunit.html.HtmlInlineFrame;
import com.gargoylesoftware.htmlunit.html.HtmlItalic;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.HtmlScript;
import com.google.common.collect.Lists;
import com.google.common.io.Files;

import fr.putnami.pwt.core.mvp.client.MvpPlace;
import fr.putnami.pwt.doc.client.page.binding.DataBindingPlace;
import fr.putnami.pwt.doc.client.page.bootstrap.BootstrapPlace;
import fr.putnami.pwt.doc.client.page.codeeditor.CodeEditorPlace;
import fr.putnami.pwt.doc.client.page.components.ComponentsPlace;
import fr.putnami.pwt.doc.client.page.download.DownloadPlace;
import fr.putnami.pwt.doc.client.page.errors.ErrorsPlace;
import fr.putnami.pwt.doc.client.page.i18n.InternationalizationPlace;
import fr.putnami.pwt.doc.client.page.layout.LayoutsPlace;
import fr.putnami.pwt.doc.client.page.navigation.NavigationPlace;
import fr.putnami.pwt.doc.client.page.sample.addressbook.AddressBookPlace;
import fr.putnami.pwt.doc.client.page.sample.all.SamplesPlace;
import fr.putnami.pwt.doc.client.page.sample.table.ContactsTablePlace;
import fr.putnami.pwt.doc.client.page.server.ServerCallsPlace;
import fr.putnami.pwt.doc.client.page.soon.ComingSoonPlace;
import fr.putnami.pwt.doc.client.page.starting.GettingStartedPlace;
import fr.putnami.pwt.doc.client.page.welcome.WelcomePlace;

public class ExtratHtmlStatics {

	private static final String targetHost = "http://pwt.putnami.org/";
	private static final String host = "http://localhost:8080/pwt-doc/";
	private static final String outFolder = "build/htmlStatics/";
	private static final File siteMap = new File(outFolder + "sitemap.txt");
	private static WebClient webClient;

	public static void main(String[] args) {
		webClient = new WebClient(BrowserVersion.CHROME);

		siteMap.delete();
		new File(outFolder).mkdirs();
		try {
			extractPage(WelcomePlace.class);
			extractPage(GettingStartedPlace.class);
			extractPage(BootstrapPlace.class);
			extractPage(LayoutsPlace.class);
			extractPage(ComponentsPlace.class);
			extractPage(DataBindingPlace.class);
			extractPage(InternationalizationPlace.class);
			extractPage(NavigationPlace.class);
			extractPage(ServerCallsPlace.class);
			extractPage(ErrorsPlace.class);
			extractPage(CodeEditorPlace.class);
			extractPage(SamplesPlace.class);
			extractPage(ContactsTablePlace.class);
			extractPage(AddressBookPlace.class);
			extractPage(ComingSoonPlace.class);
			extractPage(DownloadPlace.class);

			Files.copy(new File(outFolder + "Welcome.html"), new File(outFolder + "index.html"));
		}
		catch (FailingHttpStatusCodeException | IOException e) {
			throw new RuntimeException(e);
		}

	}

	private static void extractPage(Class<? extends MvpPlace> place)
			throws FailingHttpStatusCodeException, MalformedURLException, IOException {

		String tokenName = null;
		String url = host;
		if (place != null) {
			tokenName = place.getSimpleName().replace("Place", "");
			url += "#!" + tokenName;
		}
		HtmlPage page = webClient.getPage(url);
		//		for (DomNode node : page.getHead().getChildren()) {
		//			if ("script".equals(node.getNodeName())) {
		//				node.remove();
		//			}
		//		}

		// Elements to remove
		List<DomNode> elementsToRemove = Lists.newArrayList();

		// Add an id to the putnami-showcase div
		for (DomNode node : page.getBody().getChildren()) {
			if (HtmlDivision.TAG_NAME.equals(node.getNodeName())) {
				NamedNodeMap attributes = node.getAttributes();
				DomAttr classAttr = (DomAttr) attributes.getNamedItem("class");
				if (classAttr != null
						&& classAttr.getValue() != null && classAttr.getValue().contains("putnami-showcase")) {
					DomElement domElement = (DomElement) node;
					domElement.setAttribute("id", "pwt-static-content");
				}
			}
			else if (HtmlInlineFrame.TAG_NAME.equals(node.getNodeName())) {
				// Remove the iframe
				elementsToRemove.add(node);
			}
		}

		// Remove the permutation script
		for (DomElement elem : page.getHead().getElementsByTagName(HtmlScript.TAG_NAME)) {
			HtmlScript scriptElem = (HtmlScript) elem;
			if (scriptElem.getAttribute("src") != null) {
				if (scriptElem.getAttribute("src").endsWith(".cache.js") || scriptElem.getAttribute("src").endsWith("google-analytics.com/analytics.js")) {
					elementsToRemove.add(scriptElem);
				}
			}
		}

		// Remove all the <i> tag because GWT doesn't handle them properly
		elementsToRemove.addAll(page.getBody().getElementsByTagName(HtmlItalic.TAG_NAME));

		// Remove all the needed elements
		for (DomNode elem : elementsToRemove) {
			elem.remove();
		}

		String pageData = page.asXml().replace("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<!DOCTYPE html>");
		pageData = pageData.replaceAll(host, "");
		pageData = pageData.replaceAll("#\\!([a-zA-Z]*)", targetHost + "$1.html");

		String outFile = outFolder;
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
