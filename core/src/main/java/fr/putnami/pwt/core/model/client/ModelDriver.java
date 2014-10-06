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
package fr.putnami.pwt.core.model.client;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.logging.Logger;

import fr.putnami.pwt.core.editor.client.Context;
import fr.putnami.pwt.core.editor.client.Driver;
import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.EditorComposite;
import fr.putnami.pwt.core.editor.client.EditorLeaf;
import fr.putnami.pwt.core.editor.client.Error;
import fr.putnami.pwt.core.editor.client.Path;
import fr.putnami.pwt.core.editor.client.Visitor;
import fr.putnami.pwt.core.editor.client.event.DataValidationEvent;
import fr.putnami.pwt.core.editor.client.event.DirtyEvent;
import fr.putnami.pwt.core.editor.client.event.FlushErrorEvent;
import fr.putnami.pwt.core.editor.client.event.FlushSuccessEvent;
import fr.putnami.pwt.core.editor.client.event.ResetDisplayEvent;
import fr.putnami.pwt.core.editor.client.helper.MessageHelper;
import fr.putnami.pwt.core.editor.client.impl.SimpleEditorContext;
import fr.putnami.pwt.core.editor.client.util.PathUtils;
import fr.putnami.pwt.core.model.client.factory.ContextFactory;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.model.client.visitor.BinderVisitor;
import fr.putnami.pwt.core.model.client.visitor.DirtyEventRegistrerVisitor;
import fr.putnami.pwt.core.model.client.visitor.DriverSetterVisitor;
import fr.putnami.pwt.core.model.client.visitor.EditorFactoryVisitor;
import fr.putnami.pwt.core.model.client.visitor.EnumValuesVisitor;
import fr.putnami.pwt.core.model.client.visitor.ErrorBinderVisitor;
import fr.putnami.pwt.core.model.client.visitor.FlusherVisitor;
import fr.putnami.pwt.core.model.client.visitor.HtmlForVisitor;
import fr.putnami.pwt.core.model.client.visitor.InputValidatorVisitor;
import fr.putnami.pwt.core.model.client.visitor.LabelFactoryVisitor;
import fr.putnami.pwt.core.model.client.visitor.LabelRendererVisitor;
import fr.putnami.pwt.core.model.client.visitor.MessageHelperContainerVisitor;
import fr.putnami.pwt.core.model.client.visitor.ModelInitializerVisitor;
import fr.putnami.pwt.core.model.client.visitor.PlaceholderRendererVisitor;
import fr.putnami.pwt.core.model.client.visitor.ReadonlyAspectVisitor;
import fr.putnami.pwt.core.model.client.visitor.TooltipRendererVisitor;

public class ModelDriver<T> implements Driver<T> {

  public static class DefaultContextFactory implements ContextFactory {
    @Override
    public <B extends Editor> Context<B> createContext(ModelDriver<?> driver, Context<?> parentContext, B editor) {
      Context<B> context = (Context<B>) driver.getContext(editor);
      if (context != null) {
        return context;
      }

      String path = editor.getPath();
      if (parentContext != null) {
        if (path == null || path.length() == 0) {
          path = parentContext.getPath().toString();
        }
        else if (parentContext.getPath().toString().length() > 0) {
          if (path.startsWith("[") && path.endsWith("]")) {
            path = parentContext.getPath().toString() + path;
          }
          else {
            path = parentContext.getPath().toString() + Path.SEPARATOR_PATH + path;
          }
        }
      }
      if (driver.getRootEditor() == editor) {
        path = null;
      }

      context = new SimpleEditorContext<B>(driver, parentContext, editor, PathUtils.evalPath(path));
      driver.addContext(context);

      driver.accept(new ModelInitializerVisitor(), context);
      driver.accept(new DriverSetterVisitor(), context);
      driver.accept(new LabelFactoryVisitor(), context);
      driver.accept(new EditorFactoryVisitor(), context);
      driver.accept(new LabelRendererVisitor(), context);
      driver.accept(new PlaceholderRendererVisitor(), context);
      driver.accept(new MessageHelperContainerVisitor(), context);
      driver.accept(new TooltipRendererVisitor(), context);
      driver.accept(new InputValidatorVisitor(), context);
      driver.accept(driver.htmlForVisitor, context);
      driver.accept(new DirtyEventRegistrerVisitor(driver.dirtyHandler), context);
      driver.accept(new EnumValuesVisitor(), context);
      driver.accept(new ReadonlyAspectVisitor(), context);

      for (Visitor visitor : Iterables.filter(driver.visitors, Visitor.VisitorTrigger.INITALIZE)) {
        driver.accept(visitor, context);
      }

      if (editor instanceof EditorComposite && (parentContext == null || !(editor instanceof EditorLeaf))) {
        for (Editor child : ((EditorComposite) editor).getEditors()) {
          createContext(driver, context, child);
        }
      }

      return context;
    }
  }

  private final HtmlForVisitor htmlForVisitor = new HtmlForVisitor();

  private static final Logger LOGGER = Logger.getLogger(ModelDriver.class.getName());

  private final Model<T> model;
  private final List<Context<?>> contexts = Lists.newArrayList();
  private final List<Visitor> visitors = Lists.newArrayList();

  private final List<Error> errors = Lists.newArrayList();

  boolean autoFlush = false;
  private final DirtyEvent.Handler dirtyHandler = new DirtyEvent.Handler() {

    @Override
    public void onDirtyEvent(DirtyEvent event) {
      ModelDriver.this.dirty = true;
      if (autoFlush) {
        flush();
      }
      else if (event.getEditor() != rootEditor) {
        DirtyEvent.fire(rootEditor);
      }
    }
  };

  private MessageHelper messageHelper;
  private Editor rootEditor;
  private Context<Editor> rootContext;

  private boolean dirty;

  private T value;
  private T displayedValue;

  public ModelDriver(Model<T> model) {
    this.model = model;
  }

  public Model<T> getModel() {
    return this.model;
  }

  public T getValue() {
    return this.value;
  }

  public T getDisplayedValue() {
    return displayedValue;
  }

  public void setDisplayedValue(T displayedValue) {
    this.displayedValue = displayedValue;
  }

  public void setValue(T value) {
    this.value = value;
  }

  public boolean isAutoFlush() {
    return autoFlush;
  }

  public void setAutoFlush(boolean autoFlush) {
    this.autoFlush = autoFlush;
  }

  public Context<Editor> getRootContext() {
    return rootContext;
  }

  @Override
  public <E extends Editor> E getRootEditor() {
    return (E) rootEditor;
  }

  @Override
  public Iterable<Error> getErrors() {
    return this.errors == null ? Collections.EMPTY_LIST : Iterables.unmodifiableIterable(this.errors);
  }

  @Override
  public boolean hasErrors() {
    return this.errors != null && this.errors.size() > 0;
  }

  @Override
  public boolean isDirty() {
    return this.dirty;
  }

  @Override
  public Collection<Visitor> getVisitors() {
    return Collections.unmodifiableList(this.visitors);
  }

  public Context<?> getContext(final Editor editor) {
    return Iterables.find(contexts, new Predicate<Context<?>>() {
      @Override
      public boolean apply(Context<?> context) {
        return context.getEditor() == editor;
      }
    }, null);

  }

  public boolean removeContext(final Context<?> context) {
    if (contexts.remove(context)) {
      List<Context<?>> children = Lists.newArrayList();
      for (Context<?> child : contexts) {
        if (context.equals(child.getParentContext())) {
          children.add(context);
        }
      }
      for (Context<?> childContext : children) {
        removeContext(childContext);
      }
      return true;
    }
    return false;
  }

  public boolean addContext(Context<?> context) {
    if (context != null && !contexts.contains(context)) {
      contexts.add(context);
      return true;
    }
    return false;
  }

  @Override
  public MessageHelper getMessageHelper() {
    return messageHelper;
  }

  public void setMessageHelper(MessageHelper messageHelper) {
    this.messageHelper = messageHelper;
  }

  @Override
  public void initialize(Editor editor, Visitor... visitors) {
    assert this.rootContext == null : "already initialized";
    this.rootEditor = editor;
    if (visitors != null) {
      Iterables.addAll(this.visitors, Lists.newArrayList(visitors));
      Collections.sort(this.visitors, new Visitor.VisitorComparator());
    }
    this.rootContext = ContextFactory.Util.get().createContext(this, null, editor);
  }

  @Override
  public boolean registerVisitor(Visitor visitor) {
    if (visitor == null) {
      return false;
    }
    this.visitors.add(visitor);
    Collections.sort(this.visitors, new Visitor.VisitorComparator());
    if (Visitor.VisitorTrigger.INITALIZE == visitor.trigerOn()) {
      return accept(visitor);
    }
    return true;
  }

  @Override
  public void edit(T value) {
    this.value = value;
    errors.clear();
    accept(new ErrorBinderVisitor(model, messageHelper, errors));
    resetDisplay();
  }

  @Override
  public void resetDisplay() {
    this.displayedValue = value;

    for (Visitor visitor : Iterables.filter(this.visitors, Visitor.VisitorTrigger.BEFORE_EDIT)) {
      this.accept(visitor);
    }

    this.accept(new BinderVisitor(this, displayedValue));

    for (Visitor visitor : Iterables.filter(this.visitors, Visitor.VisitorTrigger.AFTER_EDIT)) {
      this.accept(visitor);
    }
    ResetDisplayEvent.fire(rootEditor, displayedValue);
  }

  @Override
  public T flush() {
    errors.clear();

    T result = this.model.cloneBean(this.value);
    FlusherVisitor<T> flusher = new FlusherVisitor<T>(model, result);
    result = flusher.getTargetValue();
    this.accept(flusher);
    result = flusher.getTargetValue();
    if (flusher.hasErrors()) {
      Iterables.addAll(errors, flusher.getErrors());
    }
    else {
      DataValidationEvent validationEvent = DataValidationEvent.fire(rootEditor, result);
      errors.addAll(validationEvent.getErrors());
    }
    if (!errors.isEmpty()) {
      accept(new ErrorBinderVisitor(model, messageHelper, errors));
      FlushErrorEvent.fire(rootEditor, this.value, result, errors);
      result = this.value;
    }
    else {
      this.value = result;
      FlushSuccessEvent.fire(rootEditor, result);
    }

    for (Visitor visitor : Iterables.filter(this.visitors, Visitor.VisitorTrigger.FLUSH)) {
      this.accept(visitor);
    }

    return result;
  }

  @Override
  public boolean accept(Visitor visitor) {
    if (!visitor.beforeVisit()) {
      return false;
    }
    for (int i = 0; i < contexts.size(); i++) {
      Context<?> context = contexts.get(i);
      if (context != null && !visitor.visit(context)) {
        break;
      }
    }
    if (!visitor.afterVisit()) {
      return false;
    }
    return true;
  }

  public boolean accept(Visitor visitor, Context<?> context) {
    if (!visitor.beforeVisit()) {
      return false;
    }
    if (context == null || !visitor.visit(context)) {
      return false;
    }
    if (!visitor.afterVisit()) {
      return false;
    }
    return true;
  }
}
