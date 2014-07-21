/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.doc.client.application;

import com.google.gwt.core.client.GWT;
import com.google.gwt.dom.client.Document;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.mvp.client.MvpPlace;
import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.widget.client.GridColumn;
import fr.putnami.pwt.core.widget.client.Header;
import fr.putnami.pwt.core.widget.client.NavSpy;
import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;

public abstract class Page<P extends MvpPlace> extends Composite implements View<P>, HasTableOfContent {

	interface Binder extends UiBinderLocalized<Widget, PageLayout> {

		Binder BINDER = GWT.create(Binder.class);
	}

	public static class PageLayout {
		@UiField
		NavSpy tableOfContent;
		@UiField
		GridColumn headerContainer;
		@UiField
		GridColumn contentContainer;
	}

	private final PageLayout pageLayout = new PageLayout();

	@UiField(provided = true)
	public final NavSpy tableOfContent;
	@UiField
	public Header header;
	@UiField
	public Widget content;
	
	private Widget page;

	public Page() {
		initWidget(Binder.BINDER.createAndBindUi(pageLayout));
		
		
		this.tableOfContent = pageLayout.tableOfContent;
		page = (Widget)getBinder().createAndBindUi(this);
			pageLayout.headerContainer.add(header);
		pageLayout.contentContainer.add(content);
		tableOfContent.redraw();
	}

	@Override
	public void present(P place) {
		String title = page.getElement().getTitle();
		if(title != null && title.length()>0){
			Document.get().setTitle(title);
		}
		else{
			Document.get().setTitle("PWT - Putnami Web Toolkit");
		}
	}

	@Override
	public NavSpy getTableOfContent() {
		return tableOfContent;
	}

	protected abstract UiBinderLocalized getBinder();
}
