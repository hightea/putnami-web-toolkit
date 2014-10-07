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

import com.google.common.collect.Iterables;
import com.google.common.collect.Sets;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.Element;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.ComplexPanel;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

import java.util.Collections;
import java.util.Set;
import java.util.logging.Logger;

import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.EditorComposite;
import fr.putnami.pwt.core.editor.client.Path;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;
import fr.putnami.pwt.core.widget.client.util.WidgetUtils;

public abstract class AbstractPanel extends ComplexPanel
	implements EditorComposite, HasResponsiveVisibility {
	private final Logger logger = Logger.getLogger(this.getClass().getSimpleName());

	private final String tagName;

	private HandlerManager handlerManager;

	private Set<Editor> editorChildren;
	private String path;
	private Element element;

	public AbstractPanel(String tagName) {
		this.tagName = tagName;
		this.element = Document.get().createElement(tagName);
		this.setElement(this.element);
		StyleUtils.initStyle(this);
	}

	protected AbstractPanel(AbstractPanel source) {
		this(source.tagName);
		this.path = source.path;
		this.handlerManager = new HandlerManager(source.handlerManager, this);
		this.handlerManager.resetSinkEvents();
		StyleUtils.cloneStyle(this, source);
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

	public void cloneSourceWidgets(AbstractPanel source) {
		for (Widget widget : source) {
			this.append(WidgetUtils.cloneWidget(widget));
		}
	}

	protected void insert(Widget child, int beforeIndex, boolean domInsert) {
		this.insert(child, this.element, beforeIndex, domInsert);
	}

	@Override
	protected void insert(Widget child, com.google.gwt.user.client.Element container,
		int beforeIndex, boolean domInsert) {
		if (container != this.element) {
			this.logger
				.warning("insert(Widget child, com.google.gwt.user.client.Element container, int beforeIndex, boolean domInsert) should not be used, use insert(Widget child, int beforeIndex, boolean domInsert) instead.");
		}
		super.insert(child, container, beforeIndex, domInsert);
	}

	@Override
	protected void add(Widget child, Element container) {
		if (child != null) {
			super.add(child, container);
		}
	}

	@Override
	protected void add(Widget child, com.google.gwt.user.client.Element container) {
		if (container != this.element) {
			this.logger
				.warning("void add(Widget child, com.google.gwt.user.client.Element container) should not be used");
		}
		this.addEditor(child);
		if (child != null) {
			super.add(child, container);
		}
	}

	/**
	 * This method should not be called and only be overriden to use with UiBinder. In other cases,
	 * use {@link AbstractEditablePanel#append(IsWidget)}.
	 */
	@Override
	public void add(IsWidget child) {
		throw new UnsupportedOperationException(
			"To add a widget to the panel use the append(IsWidget) method");
	}

	/**
	 * This method should not be overriden nor called. Override
	 * {@link AbstractEditablePanel#add(IsWidget)} instead or call
	 * {@link AbstractEditablePanel#append(IsWidget)} instead.
	 */
	@Override
	public void add(Widget child) {
		this.add((IsWidget) child);
	}

	public void append(IsWidget child) {
		this.add(Widget.asWidgetOrNull(child), this.element);
	}

	@Override
	public boolean remove(Widget w) {
		if (this.editorChildren != null) {
			this.editorChildren.remove(w);
		}
		return super.remove(w);
	}

	@Override
	public Iterable<Editor> getEditors() {
		return this.editorChildren == null ? Collections.<Editor> emptySet() : Iterables
			.unmodifiableIterable(this.editorChildren);
	}

	@Override
	public String getPath() {
		return this.path == null ? Path.ROOT_PATH : this.path;
	}

	@Override
	public void setPath(String path) {
		this.path = path;
	}

	protected void addEditor(IsWidget widget) {
		if (widget instanceof Editor) {
			if (this.editorChildren == null) {
				this.editorChildren = Sets.newLinkedHashSet();
			}
			this.editorChildren.add((Editor) widget);
		}
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
