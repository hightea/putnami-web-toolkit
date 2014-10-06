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

public abstract class AbstractPanel extends ComplexPanel implements EditorComposite, HasResponsiveVisibility {
	private final Logger LOGGER = Logger.getLogger(this.getClass().getSimpleName());

	private final String tagName;

	private HandlerManager handlerManager;

	private Set<Editor> editorChildren;
	private String path;
	private Element element;

	public AbstractPanel(String tagName) {
		this.tagName = tagName;
		this.element = Document.get().createElement(tagName);
		setElement(element);
		StyleUtils.initStyle(this);
	}

	protected AbstractPanel(AbstractPanel source) {
		this(source.tagName);
		path = source.path;
		handlerManager = new HandlerManager(source.handlerManager, this);
		handlerManager.resetSinkEvents();
		StyleUtils.cloneStyle(this, source);
	}

	@Override
	protected HandlerManager createHandlerManager() {
		if (handlerManager == null) {
			handlerManager = new HandlerManager(this);
		}
		return handlerManager;
	}

	@Override
	public void sinkEvents(int eventBitsToAdd) {
		super.sinkEvents(eventBitsToAdd);
		createHandlerManager().sinkEvents(eventBitsToAdd);
	}

	@Override
	public void unsinkEvents(int eventBitsToRemove) {
		super.sinkEvents(eventBitsToRemove);
		createHandlerManager().unsinkEvents(eventBitsToRemove);
	}

	public void cloneSourceWidgets(AbstractPanel source) {
		for (Widget widget : source) {
			append(WidgetUtils.cloneWidget(widget));
		}
	}

	protected void insert(Widget child, int beforeIndex, boolean domInsert) {
		insert(child, element, beforeIndex, domInsert);
	}

	/**
	 * This method should not be called. Use {@link AbstractEditablePanel#insert(Widget, int, boolean)} instead.
	 */
	@Override
	protected void insert(Widget child, Element container, int beforeIndex, boolean domInsert) {
		super.insert(child, container, beforeIndex, domInsert);
	}

	@Override
	protected void insert(Widget child, com.google.gwt.user.client.Element container, int beforeIndex, boolean domInsert) {
		if (container != element) {
			LOGGER.warning("insert(Widget child, com.google.gwt.user.client.Element container, int beforeIndex, boolean domInsert) should not be used, use insert(Widget child, int beforeIndex, boolean domInsert) instead.");
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
		if (container != element) {
			LOGGER.warning("void add(Widget child, com.google.gwt.user.client.Element container) should not be used");
		}
		addEditor(child);
		if (child != null) {
			super.add(child, container);
		}
	}

	/**
	 * This method should not be called and only be overriden to use with UiBinder. In other cases, use {@link AbstractEditablePanel#append(IsWidget)}.
	 */
	@Override
	public void add(IsWidget child) {
		throw new UnsupportedOperationException("To add a widget to the panel use the append(IsWidget) method");
	}

	/**
	 * This method should not be overriden nor called. Override {@link AbstractEditablePanel#add(IsWidget)} instead or call
	 * {@link AbstractEditablePanel#append(IsWidget)} instead.
	 */
	@Override
	public void add(Widget child) {
		this.add((IsWidget) child);
	}

	public void append(IsWidget child) {
		this.add(asWidgetOrNull(child), element);
	}

	@Override
	public boolean remove(Widget w) {
		if (editorChildren != null) {
			editorChildren.remove(w);
		}
		return super.remove(w);
	}

	@Override
	public Iterable<Editor> getEditors() {
		return editorChildren == null ? Collections.EMPTY_SET : Iterables.unmodifiableIterable(editorChildren);
	}

	@Override
	public String getPath() {
		return path == null ? Path.ROOT_PATH : path;
	}

	@Override
	public void setPath(String path) {
		this.path = path;
	}

	protected void addEditor(IsWidget widget) {
		if (widget instanceof Editor) {
			if (editorChildren == null) {
				editorChildren = Sets.newLinkedHashSet();
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
