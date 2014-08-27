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
		if (decoratorWidget == null) {
			throw new IllegalStateException("ViewDecorator.initWidget() need to be called");
		}
		return decoratorWidget.asWidget();
	}
}
