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
package fr.putnami.pwt.core.mvp.client;

import com.google.gwt.user.client.ui.AcceptsOneWidget;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

public abstract class ViewDecorator implements AcceptsOneWidget, View {

	private IsWidget decoratorWidget;
	protected IsWidget view;

	protected void initWidget(IsWidget decoratorWidget) {
		this.decoratorWidget = decoratorWidget;
	}

	public void setView(IsWidget view) {
		this.view = view;
	}

	@Override
	public Widget asWidget() {
		if (this.decoratorWidget == null) {
			throw new IllegalStateException("ViewDecorator.initWidget() need to be called");
		}
		return this.decoratorWidget.asWidget();
	}
}
