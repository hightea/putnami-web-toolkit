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
package fr.putnami.pwt.core.widget.client.base;

import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public abstract class AbstractComposite extends Composite implements HasResponsiveVisibility,
CloneableWidget {

	private HandlerManager handlerManager;

	private String styleToClone;

	public AbstractComposite() {
	}

	protected AbstractComposite(AbstractComposite source) {
		this.handlerManager = new HandlerManager(source.handlerManager, this);
		this.handlerManager.resetSinkEvents();
		this.styleToClone = source.getElement().getClassName();
	}

	@Override
	protected void initWidget(Widget widget) {
		super.initWidget(widget);
		if (this.styleToClone != null) {
			this.getElement().setClassName(this.styleToClone);
			this.styleToClone = null;
		} else {
			StyleUtils.initStyle(this);
		}
	}

	@Override
	protected HandlerManager createHandlerManager() {
		if (this.handlerManager == null) {
			this.handlerManager = new HandlerManager(this);
		}
		return this.handlerManager;
	}

	@Override
	public void sinkEvents(int eventBitsToAdd) {
		super.sinkEvents(eventBitsToAdd);
		this.createHandlerManager().sinkEvents(eventBitsToAdd);
	}

	@Override
	public void unsinkEvents(int eventBitsToRemove) {
		super.sinkEvents(eventBitsToRemove);
		this.createHandlerManager().unsinkEvents(eventBitsToRemove);
	}

	@Override
	public void setStyleName(String style) {
		StyleUtils.addStyle(this, new SimpleStyle(style));
	}

	@Override
	public void setXsVisibility(Visibility xsVisibility) {
		StyleUtils.addStyle(this, new VisibilityStyle(TargetSize.XS, xsVisibility));
	}

	@Override
	public void setSmVisibility(Visibility smVisibility) {
		StyleUtils.addStyle(this, new VisibilityStyle(TargetSize.SM, smVisibility));
	}

	@Override
	public void setMdVisibility(Visibility mdVisibility) {
		StyleUtils.addStyle(this, new VisibilityStyle(TargetSize.MD, mdVisibility));
	}

	@Override
	public void setLgVisibility(Visibility lgVisibility) {
		StyleUtils.addStyle(this, new VisibilityStyle(TargetSize.LG, lgVisibility));
	}

	@Override
	public void setPrintVisibility(Visibility printVisibility) {
		StyleUtils.addStyle(this, new VisibilityStyle(TargetSize.PRINT, printVisibility));
	}

}
