/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.doc.client.guide;

import com.google.common.collect.Lists;
import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.i18n.client.ConstantsWithLookup;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.Widget;

import java.util.List;

import fr.putnami.pwt.core.error.client.ErrorManager;
import fr.putnami.pwt.core.error.client.widget.SimpleErrorDisplayer;
import fr.putnami.pwt.core.theme.client.CssLink;
import fr.putnami.pwt.core.theme.client.Theme;
import fr.putnami.pwt.core.theme.client.ThemeController;
import fr.putnami.pwt.core.widget.client.GridColumn;
import fr.putnami.pwt.core.widget.client.Heading;
import fr.putnami.pwt.core.widget.client.NavSpy;
import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;
import fr.putnami.pwt.doc.client.application.ApplicationConfig;
import fr.putnami.pwt.doc.client.application.Page;
import fr.putnami.pwt.doc.client.application.error.ErrorConstants;
import fr.putnami.pwt.doc.client.application.error.UmbrellaExceptionHandler;
import fr.putnami.pwt.doc.client.page.binding.DataBindingPage;
import fr.putnami.pwt.doc.client.page.bootstrap.BootstrapPage;
import fr.putnami.pwt.doc.client.page.codeeditor.CodeEditorPage;
import fr.putnami.pwt.doc.client.page.components.ComponentsPage;
import fr.putnami.pwt.doc.client.page.errors.ErrorsPage;
import fr.putnami.pwt.doc.client.page.i18n.InternationalizationPage;
import fr.putnami.pwt.doc.client.page.layout.LayoutPage;
import fr.putnami.pwt.doc.client.page.navigation.NavigationPage;
import fr.putnami.pwt.doc.client.page.server.ServerCallsPage;
import fr.putnami.pwt.doc.client.page.soon.ComingSoonPage;
import fr.putnami.pwt.doc.client.page.starting.GettingStartedPage;
import fr.putnami.pwt.doc.client.page.welcome.WelcomePage;
import fr.putnami.pwt.plugin.ga.client.GoogleAnalytics;

public class ReferenceGuide extends Composite implements EntryPoint {
	interface Binder extends UiBinderLocalized<Widget, ReferenceGuide> {
		Binder BINDER = GWT.create(Binder.class);
	}

	class Chapter {
		Chapter parent;
		List<Chapter> children;
		int level = 0;
		Heading heading;

		public Chapter(int offset, Heading heading) {
			super();
			this.level = offset;
			if (heading != null) {
				this.level += heading.getLevel();
			}
			this.heading = heading;
		}

		void addChildren(Chapter child) {
			if (this.children == null) {
				this.children = Lists.newArrayList();
			}
			child.parent = this;
			this.children.add(child);
		}

		boolean isDeeper(Chapter other) {
			return other.level > this.level;
		}

		boolean isBrother(Chapter other) {
			return other.level == this.level;
		}

		void draw(StringBuffer sb) {
			if (this.heading != null) {
				String name = "";
				Chapter cursor = this;
				Chapter parentChapter = this.parent;
				int deep = 0;
				while (parentChapter != null) {
					deep++;
					int index = parentChapter.children.indexOf(cursor) + 1;
					name = index + "." + name;
					cursor = parentChapter;
					parentChapter = cursor.parent;
				}
				name += " " + this.heading.getText();
				this.heading.setHTML("<a name=\"" + name + "\" ></a>" + name);

				if (deep > ReferenceGuide.MAX_TABLE_LEVEL) {
					return;
				}
				sb.append("<li><a href=\"#").append(name).append("\">").append(name).append("</a>\n");
			}
			if (this.children != null) {
				sb.append("<ul>\n");
				for (Chapter chapter : this.children) {
					chapter.draw(sb);
				}
				sb.append("</ul>\n");
			}
			if (this.heading != null) {
				sb.append("</li>\n");
			}
		}
	}

	private static final int MAX_TABLE_LEVEL = 3;

	Chapter root = new Chapter(0, null);
	Chapter current = this.root;

	@UiField
	GridColumn tableOfContent;
	@UiField
	GridColumn contentContainer;

	public ReferenceGuide() {
		this.initWidget(Binder.BINDER.createAndBindUi(this));

		Theme theme = new Theme();
		theme.addLink(new CssLink("theme/doc/style/pwt.css", 0));
		ThemeController.get().installTheme(theme);

		SimpleErrorDisplayer errorDisplayer = new SimpleErrorDisplayer();
		errorDisplayer.setConstants((ConstantsWithLookup) GWT.create(ErrorConstants.class));
		ErrorManager.get().setErrorDisplayer(errorDisplayer);
		ErrorManager.get().registerErrorHandler(new UmbrellaExceptionHandler());

		GoogleAnalytics.init(ApplicationConfig.ANALYTICS_TRACKER_ID, ApplicationConfig.DOMAIN)
				.trackPage();

		this.addHeading("Putnami Web Toolkit", 1);
		this.addContent(new WelcomePage(), 0);
		this.addContent(new GettingStartedPage(), 0);
		this.addHeading("Look and feel", 1);
		this.addContent(new BootstrapPage(), 1);
		this.addContent(new LayoutPage(), 1);
		this.addContent(new ComponentsPage(), 1);
		this.addHeading("Framework", 1);
		this.addContent(new NavigationPage(), 1);
		this.addContent(new DataBindingPage(), 1);
		this.addContent(new InternationalizationPage(), 1);
		this.addContent(new ServerCallsPage(), 1);
		this.addContent(new ErrorsPage(), 1);
		this.addHeading("Plugins", 1);
		this.addContent(new CodeEditorPage(), 1);
		this.addContent(new ComingSoonPage(), 0);

		StringBuffer tableContentBuffer = new StringBuffer();
		this.root.draw(tableContentBuffer);
		this.tableOfContent.getElement().setInnerHTML(tableContentBuffer.toString());
	}

	private void addHeading(String title, int level) {
		Heading heading = new Heading(level);
		heading.setText(title);
		this.contentContainer.add(heading);
		this.addChapter(heading, 0);
	}

	private void addContent(Widget content, int offset) {

		NavSpy subNav = null;
		// if (content instanceof Page) {
		// subNav = ((Page) content).tableOfContent;
		// }
		// if (subNav != null) {
		// for (Heading heading : subNav.getHeadings()) {
		// addChapter(heading, offset);
		// }
		// }
		if (content instanceof Page) {
			this.contentContainer.add(((Page) content).header);
			this.contentContainer.add(((Page) content).content);
		} else {
			this.contentContainer.add(content);
		}
	}

	private void addChapter(Heading heading, int offset) {
		int level = offset + heading.getLevel();
		Chapter chapter = new Chapter(offset, heading);

		if (this.current.isBrother(chapter)) {
			this.current.parent.addChildren(chapter);
			this.current = chapter;
		} else if (this.current.isDeeper(chapter)) {
			this.current.addChildren(chapter);
			this.current = chapter;
		} else {
			while (!this.current.isBrother(chapter)) {
				this.current = this.current.parent;
			}
			if (this.current.isBrother(chapter)) {
				this.current.parent.addChildren(chapter);
				this.current = chapter;
			}
		}
	}

	@Override
	public void onModuleLoad() {
		RootPanel.get().add(this);
	}
}
