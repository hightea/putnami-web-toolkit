package fr.putnami.pwt.core.mvp.client;

import com.google.gwt.user.client.ui.AcceptsOneWidget;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

public abstract class ViewDecorator implements AcceptsOneWidget, IsWidget {

	private IsWidget decoratorWidget;

	protected void initWidget(IsWidget decoratorWidget) {
		this.decoratorWidget = decoratorWidget;
	}

	@Override
	public Widget asWidget() {
		if (decoratorWidget == null) {
			throw new IllegalStateException("ViewDecorator.initWidget() need to be called");
		}
		return decoratorWidget.asWidget();
	}

}
