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
import fr.putnami.pwt.core.widget.client.HTMLPanel;
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
	OneWidgetPanel container;

	public DocumentationDisplay() {
		this.initWidget(Binder.BINDER.createAndBindUi(this));

		Window.addResizeHandler(new ResizeHandler() {

			@Override
			public void onResize(ResizeEvent event) {
				DocumentationDisplay.this.redraw(false);
			}
		});

		Scheduler.get().scheduleDeferred(new ScheduledCommand() {

			@Override
			public void execute() {
				DocumentationDisplay.this.redraw(true);
			}
		});
	}

	@Override
	public void setWidget(IsWidget w) {
		if (w != null) {
			this.container.setWidget(w);
			this.redraw(true);
		}
	}

	private void redraw(boolean autoScroll) {
		this.viewContainer.getElement().getStyle().clearHeight();
		int height = this.getWidget().getElement().getClientHeight();

		if (height < Window.getClientHeight()) {
			this.viewContainer.getElement().getStyle().setHeight(Window.getClientHeight() - height, Unit.PX);
		}
		if (autoScroll) {
			String historyToken = History.getToken();
			if (!Strings.isNullOrEmpty(historyToken)) {
				int top = this.affixMenu.getPinnedOffset();
				Window.scrollTo(Window.getScrollLeft(), top);
			} else {
				Window.scrollTo(Window.getScrollLeft(), 0);
			}
		}
	}

}
