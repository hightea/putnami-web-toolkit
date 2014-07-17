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
package fr.putnami.pwt.doc.client.application;

import com.google.common.base.Strings;
import com.google.gwt.core.client.GWT;
import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.ScheduledCommand;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.event.logical.shared.ResizeEvent;
import com.google.gwt.event.logical.shared.ResizeHandler;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.user.client.History;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.AcceptsOneWidget;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.widget.client.Affix;
import fr.putnami.pwt.core.widget.client.Container;
import fr.putnami.pwt.core.widget.client.HTMLPanel;
import fr.putnami.pwt.core.widget.client.NavSpy;
import fr.putnami.pwt.core.widget.client.OneWidgetPanel;
import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;

public class DocumentationDisplay extends Composite implements AcceptsOneWidget {

	interface Binder extends UiBinderLocalized<Widget, DocumentationDisplay> {
		Binder BINDER = GWT.create(Binder.class);
	}

	@UiField
	Affix affixMenu;
	@UiField
	HTMLPanel viewContainer;
	@UiField
	Affix tableOfContentAffix;

	@UiField
	OneWidgetPanel noPageContainer;
	@UiField
	Container pageContainer;
	@UiField
	NavSpy tableOfContent;
	@UiField
	OneWidgetPanel headerContainer;
	@UiField
	OneWidgetPanel contentContainer;

	public DocumentationDisplay() {
		initWidget(Binder.BINDER.createAndBindUi(this));

		Window.addResizeHandler(new ResizeHandler() {

			@Override
			public void onResize(ResizeEvent event) {
				redraw(false);
			}
		});

		Scheduler.get().scheduleDeferred(new ScheduledCommand() {

			@Override
			public void execute() {
				redraw(true);
			}
		});
	}

	@Override
	public void setWidget(IsWidget w) {
		noPageContainer.clear();
		if (w instanceof Page) {
			Page page = (Page) w;
			noPageContainer.setVisible(false);
			pageContainer.setVisible(true);

			headerContainer.setWidget(page.header);
			contentContainer.setWidget(page.content);
		}
		else {
			noPageContainer.setVisible(true);
			pageContainer.setVisible(false);

			headerContainer.setWidget(null);
			contentContainer.setWidget(null);
			noPageContainer.add(w);
		}

		tableOfContent.redraw();
		redraw(true);
	}

	private void redraw(boolean autoScroll) {
		viewContainer.getElement().getStyle().clearHeight();
		int height = getWidget().getElement().getClientHeight();

		if (height < Window.getClientHeight()) {
			viewContainer.getElement().getStyle().setHeight(Window.getClientHeight() - height, Unit.PX);
		}
		if (autoScroll) {
			String historyToken = History.getToken();
			if (!Strings.isNullOrEmpty(historyToken)) {
				int top = affixMenu.getPinnedOffset();
				Window.scrollTo(Window.getScrollLeft(), top);
			}
			else {
				Window.scrollTo(Window.getScrollLeft(), 0);
			}
		}
		tableOfContentAffix.reset();
	}

}
