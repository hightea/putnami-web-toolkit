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
package fr.putnami.pwt.doc.client.application;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.inject.client.annotation.Templated;
import fr.putnami.pwt.core.mvp.client.ViewDecorator;
import fr.putnami.pwt.core.widget.client.Affix;
import fr.putnami.pwt.core.widget.client.Container;
import fr.putnami.pwt.core.widget.client.NavSpy;
import fr.putnami.pwt.core.widget.client.OneWidgetPanel;
import fr.putnami.pwt.doc.client.social.widget.SocialBar;

@Templated
public class SummaryDecorator extends ViewDecorator {

	public static SummaryDecorator get() {
		return GWT.create(SummaryDecorator.class);
	}

	@UiField
	Affix tableOfContentAffix;

	@UiField
	Container pageContainer;
	@UiField
	NavSpy tableOfContent;
	@UiField
	OneWidgetPanel headerContainer;
	@UiField
	OneWidgetPanel contentContainer;
	@UiField
	SocialBar socialBar;

	@Override
	public void setWidget(IsWidget w) {
		if (w instanceof Page) {
			Page page = (Page) w;
			this.headerContainer.setWidget(page.header);
			this.contentContainer.setWidget(page.content);
		}
		this.tableOfContent.redraw();
	}

}
